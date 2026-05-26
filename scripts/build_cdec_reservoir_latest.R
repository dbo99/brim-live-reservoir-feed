# ==== build_cdec_reservoir_latest.R ==========================================
#
# PURPOSE:
#   Fetch the most recent CDEC reservoir storage table and write a small GeoJSON
#   feed for BRIM to load when the map is opened.
#
# OUTPUTS:
#   docs/data/cdec_reservoir_latest.geojson
#   docs/data/cdec_reservoir_latest_summary.json
#
# DESIGN:
#   - CDEC is the live/quasi-live source for current storage values.
#   - The static station index from BRIM provides coordinates and metadata.
#   - The output is static GeoJSON, suitable for GitHub Pages or another simple
#     public file host.
#   - BRIM should treat the feed as stale if feed_build_age or obs_age is too old.
#
# HOW TO RUN LOCALLY FROM THIS REPOSITORY:
#   Rscript scripts/build_cdec_reservoir_latest.R
#
# REQUIRED INPUT:
#   data/input/cdec_reservoir_station_index.csv
#
# NOTES:
#   CDEC's displayed reservoir table is provisional and subject to change.
#   CDEC report times are parsed as Pacific local time using
#   America/Los_Angeles.  CDEC documentation and tables may say PST, but the
#   displayed web-table times behave like local Pacific clock time in daylight
#   saving months.  The raw display string is preserved as
#   obs_datetime_cdec_display.
# ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c(
  "curl", "dplyr", "readr", "stringr", "lubridate", "jsonlite", "tibble", "purrr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running locally, or let the GitHub Action install them."
  )
}

suppressPackageStartupMessages({
  library(curl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(tibble)
  library(purrr)
})

# ---- 2. Paths and constants -------------------------------------------------

station_index_csv <- Sys.getenv(
  "CDEC_STATION_INDEX_CSV",
  unset = "data/input/cdec_reservoir_station_index.csv"
)

cnrfc_availability_csv <- Sys.getenv(
  "CNRFC_RESERVOIR_PRODUCT_AVAILABILITY_CSV",
  unset = "data/input/cnrfc_reservoir_product_availability.csv"
)

cnrfc_role_overrides_csv <- Sys.getenv(
  "CNRFC_RESERVOIR_ROLE_OVERRIDES_CSV",
  unset = "data/input/cnrfc_reservoir_role_overrides.csv"
)

capacity_overrides_csv <- Sys.getenv(
  "CDEC_RESERVOIR_CAPACITY_OVERRIDES_CSV",
  unset = "data/input/cdec_reservoir_capacity_overrides.csv"
)

out_geojson <- Sys.getenv(
  "CDEC_RESERVOIR_GEOJSON",
  unset = "docs/data/cdec_reservoir_latest.geojson"
)

out_summary <- Sys.getenv(
  "CDEC_RESERVOIR_SUMMARY_JSON",
  unset = "docs/data/cdec_reservoir_latest_summary.json"
)

cdec_getall_url <- Sys.getenv(
  "CDEC_STORAGE_GETALL_URL",
  unset = "https://cdec.water.ca.gov/dynamicapp/getAll?sens_num=15"
)

cdec_res_daily_url <- Sys.getenv(
  "CDEC_RES_DAILY_URL",
  ## Daily reservoir summary ending at midnight.  This is not a replacement for
  ## the latest sensor-15 table, but it adds useful hydrologic context such as
  ## midnight storage, daily storage change, percent capacity, average storage,
  ## inflow/outflow, and prior-year storage where CDEC reports them.
  unset = "https://cdec.water.ca.gov/reportapp/javareports?name=RES"
)

usace_ca_plots_url <- Sys.getenv(
  "USACE_CA_RESERVOIR_PLOTS_URL",
  unset = "https://www.spk-wc.usace.army.mil/plots/california.html"
)

feed_build_time_utc <- format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

min_latest_rows_to_publish <- suppressWarnings(as.integer(Sys.getenv(
  "CDEC_MIN_LATEST_ROWS_TO_PUBLISH",
  unset = "30"
)))

if (is.na(min_latest_rows_to_publish) || min_latest_rows_to_publish < 1) {
  min_latest_rows_to_publish <- 30L
}

allow_degraded_publish <- tolower(Sys.getenv(
  "CDEC_ALLOW_DEGRADED_PUBLISH",
  unset = "false"
)) %in% c("true", "1", "yes", "y")

# ---- 3. Helpers -------------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_empty_role_overrides <- function() {
  tibble::tibble(
    cdec_id = character(),
    cnrfc_obs_id_override = character(),
    cnrfc_inflow_id_override = character(),
    cnrfc_ensemble_id_override = character(),
    cnrfc_release_id_override = character(),
    cnrfc_role_override_note = character()
  )
}

pt_read_role_overrides <- function(path) {
  if (!file.exists(path)) {
    message("CNRFC role-override CSV not found; continuing without role-specific overrides: ", path)
    return(pt_empty_role_overrides())
  }

  x <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- c("cdec_id", "cnrfc_obs_id", "cnrfc_inflow_id", "cnrfc_ensemble_id", "cnrfc_release_id")
  missing_cols <- setdiff(required_cols, names(x))

  if (length(missing_cols) > 0) {
    stop("CNRFC role-override CSV is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!"note" %in% names(x)) {
    x$note <- NA_character_
  }

  x |>
    dplyr::mutate(
      cdec_id = toupper(pt_chr(.data$cdec_id)),
      cnrfc_obs_id_override = toupper(pt_chr(.data$cnrfc_obs_id)),
      cnrfc_inflow_id_override = toupper(pt_chr(.data$cnrfc_inflow_id)),
      cnrfc_ensemble_id_override = toupper(pt_chr(.data$cnrfc_ensemble_id)),
      cnrfc_release_id_override = toupper(pt_chr(.data$cnrfc_release_id)),
      cnrfc_role_override_note = as.character(.data$note)
    ) |>
    dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
    dplyr::select(
      "cdec_id",
      "cnrfc_obs_id_override",
      "cnrfc_inflow_id_override",
      "cnrfc_ensemble_id_override",
      "cnrfc_release_id_override",
      "cnrfc_role_override_note"
    ) |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
}

pt_empty_capacity_overrides <- function() {
  tibble::tibble(
    cdec_id = character(),
    capacity_af_override = numeric(),
    capacity_source_name = character(),
    capacity_source_url = character(),
    capacity_source_date_accessed = character(),
    capacity_confidence = character(),
    capacity_note = character()
  )
}

pt_read_capacity_overrides <- function(path) {
  if (!file.exists(path)) {
    message("CDEC capacity-override CSV not found; continuing without static capacity overrides: ", path)
    return(pt_empty_capacity_overrides())
  }

  x <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- c("cdec_id", "capacity_af")

  missing_cols <- setdiff(required_cols, names(x))

  if (length(missing_cols) > 0) {
    stop("CDEC capacity-override CSV is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  optional_cols <- c(
    "capacity_source_name",
    "capacity_source_url",
    "capacity_source_date_accessed",
    "capacity_confidence",
    "capacity_note"
  )

  for (nm in optional_cols) {
    if (!nm %in% names(x)) {
      x[[nm]] <- NA_character_
    }
  }

  x |>
    dplyr::mutate(
      cdec_id = toupper(pt_chr(.data$cdec_id)),
      capacity_af_override = pt_num(.data$capacity_af),
      capacity_source_name = as.character(.data$capacity_source_name),
      capacity_source_url = as.character(.data$capacity_source_url),
      capacity_source_date_accessed = as.character(.data$capacity_source_date_accessed),
      capacity_confidence = as.character(.data$capacity_confidence),
      capacity_note = as.character(.data$capacity_note)
    ) |>
    dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
    dplyr::filter(!is.na(.data$capacity_af_override), .data$capacity_af_override > 0) |>
    dplyr::select(
      "cdec_id",
      "capacity_af_override",
      "capacity_source_name",
      "capacity_source_url",
      "capacity_source_date_accessed",
      "capacity_confidence",
      "capacity_note"
    ) |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
}

pt_num <- function(x) {
  ## CDEC table cells can include units and punctuation, for example:
  ##   "4,286,921 AF"
  ##   "1067'"
  ## Keep digits, decimal points, and minus signs only before numeric conversion.
  ## This helper is intentionally used only on fields that are already expected
  ## to be numeric; do not use it for dates or station IDs.
  x <- as.character(x)
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub("[^0-9.-]", "", x)
  x[x == "" | x == "-" | x == "."] <- NA_character_
  suppressWarnings(as.numeric(x))
}

pt_force_utf8 <- function(x) {
  ## Some CNRFC product pages include bytes that are not valid UTF-8.  Those
  ## bytes can trigger errors such as "invalid multibyte string" when stringr,
  ## toupper(), or grepl() parse the product lists.  For BRIM's purposes here,
  ## the critical content is ASCII station IDs such as ANTC1, SHDC1, and NIMC1.
  ## Convert to safe UTF-8 and preserve any odd bytes as printable placeholders
  ## rather than letting one bad character break the scheduled feed build.
  x <- as.character(x)

  y <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "byte")

  bad <- is.na(y)
  if (any(bad)) {
    y[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "byte")
  }

  y[is.na(y)] <- ""
  y
}

pt_fetch_text <- function(url, label = url, timeout_sec = 30, retries = 3) {
  message("Fetching ", label, ": ", url)

  user_agent <- paste(
    "Mozilla/5.0",
    "BRIM live reservoir feed builder",
    "R",
    getRversion()
  )

  last_error <- NULL

  for (attempt in seq_len(retries)) {
    message("  attempt ", attempt, " of ", retries)

    txt <- tryCatch({
      h <- curl::new_handle(
        useragent = user_agent,
        followlocation = TRUE,
        timeout = timeout_sec,
        connecttimeout = timeout_sec,
        httpheader = c(
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Cache-Control" = "no-cache",
          "Pragma" = "no-cache"
        )
      )
      raw <- curl::curl_fetch_memory(url, handle = h)$content
      pt_force_utf8(rawToChar(raw))
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(txt) && nzchar(txt)) {
      return(txt)
    }

    Sys.sleep(1 + attempt)
  }

  stop("Could not fetch ", label, ". Last error: ", last_error)
}

pt_extract_cnrfc_product_ids <- function(html, known_ids = character()) {
  ## The CNRFC reservoir-inflow/release product pages can render IDs in links,
  ## option values, JavaScript, visible text, or embedded page data.  The safest
  ## strategy for BRIM is:
  ##   1. Search for exact known BRIM CNRFC/NWS IDs anywhere in the page text.
  ##   2. Also run broad regex extraction for ordinary id=<NWSID> patterns.
  ##   3. Intersect everything back to known_ids so unrelated NOAA/CNRFC text
  ##      cannot create reservoir popup links.
  ##
  ## This keeps popups selective: inflow/release links appear only for IDs that
  ## are currently present on the corresponding CNRFC product-list page.
  known_ids <- toupper(pt_chr(known_ids))
  known_ids <- sort(unique(known_ids[!is.na(known_ids) & known_ids != ""]))

  if (length(known_ids) == 0 || is.null(html) || !nzchar(html)) {
    return(character())
  }

  html <- pt_force_utf8(html)
  txt <- toupper(html)
  txt <- gsub("&AMP;", "&", txt, fixed = TRUE)
  txt <- gsub("&#39;|&APOS;", "'", txt, ignore.case = TRUE)
  txt <- gsub("&QUOT;", "\"", txt, fixed = TRUE)

  ## Exact known-ID scan.  This is intentionally simple and robust to CNRFC
  ## changing whether IDs appear in links, JavaScript arrays, dropdown labels,
  ## or visible text.
  ids0 <- known_ids[
    vapply(
      known_ids,
      function(id) grepl(id, txt, fixed = TRUE),
      logical(1)
    )
  ]

  ## URL/form-style patterns: id=ANTC1, ?id=ANTC1, value="ANTC1", etc.
  id_from_params <- stringr::str_match_all(
    txt,
    "(?:[?&]ID=|\\bID\\s*[=:]\\s*['\\\"]?|VALUE\\s*=\\s*['\\\"]?)([A-Z0-9]{3,8})"
  )[[1]]
  ids1 <- if (nrow(id_from_params) > 0) id_from_params[, 2] else character()

  ## General CNRFC/NWS-like IDs.  Most California CNRFC IDs are five characters
  ## such as ANTC1, SHDC1, FOLC1, NIMC1, but keep the expression broad enough
  ## for similar products.
  ids2 <- stringr::str_extract_all(
    txt,
    "\\b[A-Z0-9]{3,6}[A-Z][0-9]\\b"
  )[[1]]

  ids <- sort(unique(c(ids0, ids1, ids2)))
  ids <- ids[!is.na(ids) & ids != ""]
  intersect(ids, known_ids)
}

pt_fetch_cnrfc_product_ids <- function(url, label, known_ids) {
  ids <- tryCatch({
    html <- pt_fetch_text(url, label = label, timeout_sec = 30, retries = 2)
    pt_extract_cnrfc_product_ids(html, known_ids = known_ids)
  }, error = function(e) {
    warning("Could not fetch/parse ", label, ": ", conditionMessage(e))
    character()
  })

  message(label, " IDs matched to BRIM station index: ", length(ids))
  ids
}

pt_usace_ca_reservoir_lookup <- function(usace_url) {
  ## Static whitelist from the USACE Sacramento District "Corps and Section 7
  ## Projects in California" plots page.  These are stable project/station IDs
  ## used to show the generic USACE California plots page only where it is likely
  ## to be useful.  The page itself is the authoritative destination and contains
  ## the current plot/data links.
  tibble::tribble(
    ~cdec_id, ~usace_project_display,
    "SHA", "Shasta Dam & Lake Shasta",
    "BLB", "Black Butte Dam & Lake",
    "ORO", "Oroville Dam & Lake Oroville",
    "BUL", "New Bullards Bar Dam & Lake",
    "ENG", "Englebright Lake",
    "INV", "Indian Valley Dam & Reservoir",
    "FOL", "Folsom Dam & Lake",
    "CMN", "Camanche Dam & Reservoir",
    "NHG", "New Hogan Dam & Lake",
    "FRM", "Farmington Dam & Reservoir",
    "NML", "New Melones Dam & Lake",
    "TUL", "Tulloch Dam & Reservoir",
    "DNP", "Don Pedro Dam & Lake",
    "EXC", "New Exchequer Dam / Lake McClure",
    "LBN", "Los Banos Detention Reservoir",
    "BUR", "Burns Dam & Reservoir",
    "BAR", "Bear Dam & Reservoir",
    "OWN", "Owens Dam & Reservoir",
    "MAR", "Mariposa Dam & Reservoir",
    "BUC", "Buchanan Dam / H.V. Eastman Lake",
    "HID", "Hidden Dam / Hensley Lake",
    "MIL", "Friant Dam / Millerton Lake",
    "BDC", "Big Dry Creek Dam & Reservoir",
    "PNF", "Pine Flat Dam & Lake",
    "TRM", "Terminus Dam / Lake Kaweah",
    "SCC", "Schafer Dam / Success Lake",
    "ISB", "Isabella Dam & Lake Isabella",
    "COY", "Coyote Valley Dam / Lake Mendocino",
    "WRS", "Warm Springs Dam / Lake Sonoma",
    "DLV", "Del Valle Dam & Reservoir",
    "MRT", "Martis Creek Dam & Lake",
    "PRS", "Prosser Creek Dam & Reservoir",
    "STP", "Stampede Dam & Reservoir",
    "BOC", "Boca Dam & Reservoir"
  ) |>
    dplyr::mutate(
      usace_california_plots_url = usace_url
    )
}

pt_html_to_lines <- function(html) {
  txt <- html
  txt <- gsub("(?i)<br\\s*/?>", "\n", txt, perl = TRUE)
  txt <- gsub("(?i)</tr>|</p>|</div>|</li>", "\n", txt, perl = TRUE)
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("(?i)&nbsp;?", " ", txt, perl = TRUE)
  txt <- gsub("(?i)&(ensp|emsp);?", " ", txt, perl = TRUE)
  txt <- gsub("&#160;", " ", txt, fixed = TRUE)
  txt <- gsub("&#xa0;", " ", txt, ignore.case = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
  txt <- gsub("&#39;", "'", txt, fixed = TRUE)
  txt <- gsub("\\u00a0", " ", txt, fixed = TRUE)

  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  lines <- stringr::str_squish(lines)
  lines[nzchar(lines)]
}


pt_strip_html <- function(x) {
  x <- gsub("(?i)<br\\s*/?>", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("(?i)&nbsp;?", " ", x, perl = TRUE)
  x <- gsub("(?i)&(ensp|emsp);?", " ", x, perl = TRUE)
  x <- gsub("&#160;", " ", x, fixed = TRUE)
  x <- gsub("&#xa0;", " ", x, ignore.case = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  stringr::str_squish(x)
}

pt_extract_html_rows <- function(html) {
  ## CDEC's getAll page is an HTML table.  A line-based parser can fail because
  ## station name, ID, elevation, date/time, and value may be emitted as separate
  ## cells/lines.  Parse <tr>/<td> structure first, then fall back to text lines.
  row_matches <- stringr::str_match_all(
    html,
    stringr::regex("<tr[^>]*>(.*?)</tr>", ignore_case = TRUE, dotall = TRUE)
  )[[1]]

  if (nrow(row_matches) == 0) {
    return(list())
  }

  rows <- row_matches[, 2]

  lapply(rows, function(row_html) {
    cell_matches <- stringr::str_match_all(
      row_html,
      stringr::regex("<t[hd][^>]*>(.*?)</t[hd]>", ignore_case = TRUE, dotall = TRUE)
    )[[1]]

    if (nrow(cell_matches) == 0) {
      return(character(0))
    }

    cells <- cell_matches[, 2]
    cells <- pt_strip_html(cells)
    cells[nzchar(cells)]
  })
}

pt_is_basin_header <- function(line) {
  if (!nzchar(line)) return(FALSE)
  if (grepl("ACTIVE|REPORT|GENERATED|SORTED|STATION|DATE/TIME|VALUE|MENU|SEARCH|QUERY|PROVISIONAL|EXECUTED", line, ignore.case = TRUE)) {
    return(FALSE)
  }
  has_letters <- grepl("[A-Z]", line)
  mostly_upper <- line == toupper(line)
  short_enough <- nchar(line) <= 60
  has_letters && mostly_upper && short_enough
}

pt_parse_getall_storage <- function(html) {
  out <- list()
  current_basin <- NA_character_

  # ---- Preferred parser: HTML table rows/cells -----------------------------
  cell_rows <- pt_extract_html_rows(html)

  for (cells in cell_rows) {
    cells <- cells[nzchar(cells)]

    if (length(cells) == 1 && pt_is_basin_header(cells[1])) {
      current_basin <- stringr::str_to_title(cells[1])
      next
    }

    # Expected CDEC storage row cells:
    #   Station Name | ID | Elev. | Date/Time | Value
    if (length(cells) >= 5) {
      station_name <- cells[1]
      cdec_id <- toupper(stringr::str_squish(cells[2]))
      elev_txt <- cells[3]
      dt_txt <- stringr::str_squish(cells[4])
      value_txt <- cells[5]

      is_station_row <- grepl("^[A-Z0-9]{2,5}$", cdec_id) &&
        grepl("^\\d{2}/\\d{2}/\\d{4}\\s+\\d{2}:\\d{2}$", dt_txt) &&
        grepl("AF", value_txt, ignore.case = TRUE)

      if (is_station_row) {
        out[[length(out) + 1]] <- tibble::tibble(
          cdec_station_name_latest = stringr::str_squish(station_name),
          cdec_id = cdec_id,
          elevation_ft_latest = pt_num(elev_txt),
          obs_datetime_cdec_display = dt_txt,
          storage_af = pt_num(value_txt),
          cdec_basin_latest_table = current_basin
        )
      }
    }
  }

  if (length(out) > 0) {
    return(
      dplyr::bind_rows(out) |>
        dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
    )
  }

  # ---- Fallback parser: stripped text lines --------------------------------
  lines <- pt_html_to_lines(html)
  current_basin <- NA_character_
  out <- list()

  # Example row after stripping HTML when cells remain on one line:
  # SHASTA DAM (USBR) SHA 1067' 05/21/2026 11:00 4,053,961 AF
  row_pattern <- paste0(
    "^(.+?)\\s+",                         # station name
    "([A-Z0-9]{2,5})\\s+",                # CDEC ID
    "(-?\\d{1,5})'?\\s+",                # elevation
    "(\\d{2}/\\d{2}/\\d{4}\\s+\\d{2}:\\d{2})\\s+", # datetime
    "([0-9,.-]+)\\s+AF\\s*$"             # storage AF
  )

  for (line in lines) {
    m <- stringr::str_match(line, row_pattern)

    if (!all(is.na(m))) {
      out[[length(out) + 1]] <- tibble::tibble(
        cdec_station_name_latest = stringr::str_squish(m[, 2]),
        cdec_id = toupper(stringr::str_squish(m[, 3])),
        elevation_ft_latest = pt_num(m[, 4]),
        obs_datetime_cdec_display = stringr::str_squish(m[, 5]),
        storage_af = pt_num(m[, 6]),
        cdec_basin_latest_table = current_basin
      )
      next
    }

    if (pt_is_basin_header(line)) {
      current_basin <- stringr::str_to_title(line)
    }
  }

  if (length(out) == 0) {
    sample_lines <- utils::head(lines[grepl("\\d{2}/\\d{2}/\\d{4}", lines)], 12)
    sample_rows <- utils::head(vapply(cell_rows, paste, collapse = " | ", FUN.VALUE = character(1)), 6)
    stop(
      "No CDEC storage rows parsed from getAll table. ",
      "Sample date-bearing text lines: ", paste(sample_lines, collapse = " | "),
      "\nSample parsed HTML table rows: ", paste(sample_rows, collapse = " || ")
    )
  }

  dplyr::bind_rows(out) |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
}

pt_empty_latest_storage_tbl <- function() {
  tibble::tibble(
    cdec_station_name_latest = character(),
    cdec_id = character(),
    elevation_ft_latest = numeric(),
    obs_datetime_cdec_display = character(),
    storage_af = numeric(),
    cdec_basin_latest_table = character()
  )
}

pt_empty_res_daily_tbl <- function() {
  tibble::tibble(
    cdec_id = character(),
    midnight_station_name = character(),
    capacity_af_res = numeric(),
    midnight_elevation_ft = numeric(),
    midnight_storage_af = numeric(),
    midnight_storage_change_af = numeric(),
    midnight_pct_capacity = numeric(),
    midnight_avg_storage_af = numeric(),
    midnight_pct_average = numeric(),
    midnight_outflow_cfs = numeric(),
    midnight_inflow_cfs = numeric(),
    midnight_storage_year_ago_af = numeric(),
    midnight_basin_res_table = character(),
    midnight_report_source_url = character(),
    midnight_data_status = character()
  )
}

pt_parse_res_daily_report <- function(html, source_url = cdec_res_daily_url) {
  ## Parse the CDEC RES daily reservoir report.  The report is a daily summary
  ## ending at midnight, not a near-real-time observation table.  It is useful
  ## in BRIM as a stable hydrologic reference beside the latest sensor-15 value.
  ##
  ## Expected data-row cells:
  ##   Reservoir Name | StaID | Capacity | Elevation | Storage | Storage Change
  ##   | % Capacity | Average Storage | % Average | Outflow | Inflow
  ##   | Storage-Year Ago This Date
  out <- list()
  current_basin <- NA_character_

  cell_rows <- pt_extract_html_rows(html)

  for (cells in cell_rows) {
    cells <- cells[nzchar(cells)]

    if (length(cells) == 1 && pt_is_basin_header(cells[1])) {
      current_basin <- stringr::str_to_title(cells[1])
      next
    }

    ## Allow extra cells in case CDEC inserts links/notes, but require the first
    ## 12 core cells used by the standard RES table.
    if (length(cells) >= 12) {
      cdec_id <- toupper(stringr::str_squish(cells[2]))

      is_station_row <- grepl("^[A-Z0-9]{2,5}$", cdec_id) &&
        !grepl("STAID|RESERVOIR", cdec_id, ignore.case = TRUE)

      if (is_station_row) {
        midnight_storage_af <- pt_num(cells[5])
        status <- dplyr::case_when(
          !is.na(midnight_storage_af) ~ "CDEC RES daily midnight storage available.",
          TRUE ~ "CDEC RES daily report row present, but midnight storage is not reported."
        )

        out[[length(out) + 1]] <- tibble::tibble(
          cdec_id = cdec_id,
          midnight_station_name = stringr::str_squish(cells[1]),
          capacity_af_res = pt_num(cells[3]),
          midnight_elevation_ft = pt_num(cells[4]),
          midnight_storage_af = midnight_storage_af,
          midnight_storage_change_af = pt_num(cells[6]),
          midnight_pct_capacity = pt_num(cells[7]),
          midnight_avg_storage_af = pt_num(cells[8]),
          midnight_pct_average = pt_num(cells[9]),
          midnight_outflow_cfs = pt_num(cells[10]),
          midnight_inflow_cfs = pt_num(cells[11]),
          midnight_storage_year_ago_af = pt_num(cells[12]),
          midnight_basin_res_table = current_basin,
          midnight_report_source_url = source_url,
          midnight_data_status = status
        )
      }
    }
  }

  if (length(out) == 0) {
    warning("No CDEC RES daily reservoir rows parsed.")
    return(pt_empty_res_daily_tbl())
  }

  dplyr::bind_rows(out) |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
}


pt_make_feature <- function(row) {
  props <- as.list(row)
  props$longitude <- NULL
  props$latitude <- NULL

  list(
    type = "Feature",
    geometry = list(
      type = "Point",
      coordinates = list(as.numeric(row$longitude), as.numeric(row$latitude))
    ),
    properties = props
  )
}

# ---- 4. Read station index --------------------------------------------------

if (!file.exists(station_index_csv)) {
  stop(
    "Station index CSV not found: ", station_index_csv,
    "\nRun scripts/export_brim_cdec_station_index.R from the BRIM project first, ",
    "or copy cdec_reservoir_station_index.csv into data/input/."
  )
}

station_index_raw <- readr::read_csv(
  station_index_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

if (!"capacity_af" %in% names(station_index_raw)) {
  station_index_raw$capacity_af <- NA_character_
}

if (!"cnrfc_nws_id" %in% names(station_index_raw)) {
  station_index_raw$cnrfc_nws_id <- NA_character_
}

if (!"nws_id" %in% names(station_index_raw)) {
  station_index_raw$nws_id <- NA_character_
}

if (!"cnrfc_match_confidence" %in% names(station_index_raw)) {
  station_index_raw$cnrfc_match_confidence <- NA_character_
}

if (!"cnrfc_match_method" %in% names(station_index_raw)) {
  station_index_raw$cnrfc_match_method <- NA_character_
}

station_index <- station_index_raw |>
  dplyr::mutate(
    cdec_id = toupper(trimws(as.character(.data$cdec_id))),
    latitude = pt_num(.data$latitude),
    longitude = pt_num(.data$longitude),
    elevation_ft = pt_num(.data$elevation_ft),
    capacity_af = pt_num(.data$capacity_af),
    cnrfc_nws_id = dplyr::coalesce(
      toupper(pt_chr(.data$cnrfc_nws_id)),
      toupper(pt_chr(.data$nws_id))
    ),
    cnrfc_match_confidence = as.character(.data$cnrfc_match_confidence),
    cnrfc_match_method = as.character(.data$cnrfc_match_method)
  )

# ---- 5. Fetch and parse latest + daily reservoir storage ---------------------

## Latest / near-live CDEC sensor-15 storage table.  This table can be partial,
## especially when CDEC is missing current reservoir observations.  Do not treat
## partial coverage as a parser failure; report it clearly in the feed summary
## and in the BRIM Ops panel.
storage_html <- pt_fetch_text(cdec_getall_url, label = "CDEC latest reservoir storage table")

storage_tbl <- tryCatch(
  pt_parse_getall_storage(storage_html),
  error = function(e) {
    warning("Could not parse CDEC latest sensor-15 storage table: ", conditionMessage(e))
    pt_empty_latest_storage_tbl()
  }
)

message("CDEC latest storage rows parsed: ", nrow(storage_tbl))
message("CDEC latest storage rows with numeric storage_af: ", sum(!is.na(storage_tbl$storage_af)))

if (nrow(storage_tbl) > 0 && all(is.na(storage_tbl$storage_af))) {
  warning(
    "CDEC latest storage rows were parsed, but storage_af is NA for every row. ",
    "Continuing with daily RES report only if available."
  )
  storage_tbl <- pt_empty_latest_storage_tbl()
}

## Daily RES summary ending at midnight.  This is not live, but it provides a
## useful daily hydrologic snapshot and sometimes includes context fields that
## the latest table does not provide.
res_daily_html <- pt_fetch_text(cdec_res_daily_url, label = "CDEC RES daily reservoir report")
midnight_tbl <- pt_parse_res_daily_report(res_daily_html, source_url = cdec_res_daily_url)

message("CDEC RES daily rows parsed: ", nrow(midnight_tbl))
message("CDEC RES daily rows with midnight_storage_af: ", sum(!is.na(midnight_tbl$midnight_storage_af)))

if (
  sum(!is.na(storage_tbl$storage_af)) == 0 &&
  sum(!is.na(midnight_tbl$midnight_storage_af)) == 0
) {
  stop(
    "Neither CDEC latest storage nor CDEC RES daily midnight storage produced usable storage values. ",
    "Refusing to write an empty/degraded GeoJSON."
  )
}

# ---- 5.1 Read prebuilt CNRFC reservoir-product availability ------------------
#
# CNRFC product availability is metadata, not live storage data.  It is built by:
#   scripts/preprocess_cnrfc_reservoir_product_availability.R
#
# The GitHub Action should not scrape CNRFC product lists every six hours.  It
# reads this stable CSV instead.  Re-run the preprocessor manually when CNRFC
# products are expected to change, for example after water-year rollover, during
# summer development updates, or when a missing/extra product link is observed.

if (!file.exists(cnrfc_availability_csv)) {
  stop(
    "CNRFC reservoir product-availability CSV not found: ", cnrfc_availability_csv,
    "\nRun scripts/preprocess_cnrfc_reservoir_product_availability.R first and upload ",
    "data/input/cnrfc_reservoir_product_availability.csv to the feed repo."
  )
}

cnrfc_availability_raw <- readr::read_csv(
  cnrfc_availability_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

required_availability_cols <- c(
  "cnrfc_nws_id",
  "has_cnrfc_inflow",
  "has_cnrfc_ensemble",
  "has_cnrfc_release",
  "cnrfc_obs_url",
  "cnrfc_inflow_url",
  "cnrfc_ensemble_url",
  "cnrfc_release_url"
)

missing_availability_cols <- setdiff(required_availability_cols, names(cnrfc_availability_raw))

if (length(missing_availability_cols) > 0) {
  stop(
    "CNRFC availability CSV is missing required column(s): ",
    paste(missing_availability_cols, collapse = ", ")
  )
}

if (!"availability_build_time_utc" %in% names(cnrfc_availability_raw)) {
  cnrfc_availability_raw$availability_build_time_utc <- NA_character_
}

pt_as_logical <- function(x) {
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

cnrfc_availability_tbl <- cnrfc_availability_raw |>
  dplyr::mutate(
    cnrfc_nws_id = toupper(pt_chr(.data$cnrfc_nws_id)),
    has_cnrfc_inflow = pt_as_logical(.data$has_cnrfc_inflow),
    has_cnrfc_ensemble = pt_as_logical(.data$has_cnrfc_ensemble),
    has_cnrfc_release = pt_as_logical(.data$has_cnrfc_release),
    cnrfc_obs_url = pt_chr(.data$cnrfc_obs_url),
    cnrfc_inflow_url = pt_chr(.data$cnrfc_inflow_url),
    cnrfc_ensemble_url = pt_chr(.data$cnrfc_ensemble_url),
    cnrfc_release_url = pt_chr(.data$cnrfc_release_url),
    availability_build_time_utc = dplyr::coalesce(
      as.character(.data$availability_build_time_utc),
      NA_character_
    )
  ) |>
  dplyr::filter(!is.na(.data$cnrfc_nws_id), .data$cnrfc_nws_id != "") |>
  dplyr::distinct(.data$cnrfc_nws_id, .keep_all = TRUE)

message("CNRFC availability rows read: ", nrow(cnrfc_availability_tbl))
message("CNRFC availability inflow IDs: ", sum(cnrfc_availability_tbl$has_cnrfc_inflow, na.rm = TRUE))
message("CNRFC availability ensemble IDs: ", sum(cnrfc_availability_tbl$has_cnrfc_ensemble, na.rm = TRUE))
message("CNRFC availability release IDs: ", sum(cnrfc_availability_tbl$has_cnrfc_release, na.rm = TRUE))

pt_lookup_cnrfc_availability <- function(ids, col) {
  ids <- toupper(pt_chr(ids))
  idx <- match(ids, cnrfc_availability_tbl$cnrfc_nws_id)
  cnrfc_availability_tbl[[col]][idx]
}

role_overrides_tbl <- pt_read_role_overrides(cnrfc_role_overrides_csv)
capacity_overrides_tbl <- pt_read_capacity_overrides(capacity_overrides_csv)

message("CNRFC role override rows read: ", nrow(role_overrides_tbl))
message("CDEC capacity override rows read: ", nrow(capacity_overrides_tbl))

usace_ca_lookup <- pt_usace_ca_reservoir_lookup(usace_ca_plots_url)

# ---- 6. Join storage to station index --------------------------------------

latest_tbl <- station_index |>
  dplyr::left_join(storage_tbl, by = "cdec_id") |>
  dplyr::left_join(midnight_tbl, by = "cdec_id") |>
  dplyr::left_join(cnrfc_availability_tbl, by = "cnrfc_nws_id") |>
  dplyr::left_join(role_overrides_tbl, by = "cdec_id") |>
  dplyr::left_join(capacity_overrides_tbl, by = "cdec_id") |>
  dplyr::left_join(usace_ca_lookup, by = "cdec_id") |>
  dplyr::filter(!is.na(.data$storage_af) | !is.na(.data$midnight_storage_af)) |>
  dplyr::mutate(
    storage_maf = .data$storage_af / 1e6,
    midnight_storage_maf = .data$midnight_storage_af / 1e6,
    display_storage_af = dplyr::coalesce(.data$storage_af, .data$midnight_storage_af),
    display_storage_maf = .data$display_storage_af / 1e6,
    display_storage_source = dplyr::if_else(!is.na(.data$storage_af), "latest", "midnight_daily"),
    has_latest_storage = !is.na(.data$storage_af),
    has_midnight_storage = !is.na(.data$midnight_storage_af),
    cnrfc_obs_id_effective = dplyr::coalesce(.data$cnrfc_obs_id_override, .data$cnrfc_nws_id),
    cnrfc_inflow_id_effective = dplyr::coalesce(.data$cnrfc_inflow_id_override, .data$cnrfc_nws_id),
    cnrfc_ensemble_id_effective = dplyr::coalesce(.data$cnrfc_ensemble_id_override, .data$cnrfc_nws_id),
    cnrfc_release_id_effective = dplyr::coalesce(.data$cnrfc_release_id_override, .data$cnrfc_nws_id),
    has_cnrfc_inflow = dplyr::coalesce(
      pt_lookup_cnrfc_availability(.data$cnrfc_inflow_id_effective, "has_cnrfc_inflow"),
      FALSE
    ),
    has_cnrfc_ensemble = dplyr::coalesce(
      pt_lookup_cnrfc_availability(.data$cnrfc_ensemble_id_effective, "has_cnrfc_ensemble"),
      FALSE
    ),
    has_cnrfc_release = dplyr::coalesce(
      pt_lookup_cnrfc_availability(.data$cnrfc_release_id_effective, "has_cnrfc_release"),
      FALSE
    ),
    ## CNRFC reservoir-link conventions:
    ##   - obsRiver_hc.php is the observed river/reservoir conditions page and
    ##     can show reservoir elevation/storage detail for reservoir points.
    ##   - reservoir.php is the deterministic reservoir-inflow / graphical RVF
    ##     page and is shown only when the prebuilt availability table says the
    ##     effective inflow ID is in the CNRFC reservoir-inflow product list.
    ##   - ensembleProduct.php?prodID=3 is the HEFS/ensemble forecast page and
    ##     is shown only when the availability table says the effective ensemble
    ##     ID is in the CNRFC ensemble product list.
    ##   - reservoirRelease.php is the reservoir-release schedule page and is
    ##     shown only when the availability table says the effective release ID
    ##     is in the CNRFC reservoir-release product list.
    cnrfc_obs_url = dplyr::if_else(
      !is.na(.data$cnrfc_obs_id_effective) & .data$cnrfc_obs_id_effective != "",
      paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", .data$cnrfc_obs_id_effective),
      NA_character_
    ),
    cnrfc_inflow_url = dplyr::if_else(
      .data$has_cnrfc_inflow,
      dplyr::coalesce(
        pt_lookup_cnrfc_availability(.data$cnrfc_inflow_id_effective, "cnrfc_inflow_url"),
        paste0("https://www.cnrfc.noaa.gov/reservoir.php?id=", .data$cnrfc_inflow_id_effective)
      ),
      NA_character_
    ),
    cnrfc_ensemble_url = dplyr::if_else(
      .data$has_cnrfc_ensemble,
      dplyr::coalesce(
        pt_lookup_cnrfc_availability(.data$cnrfc_ensemble_id_effective, "cnrfc_ensemble_url"),
        paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", .data$cnrfc_ensemble_id_effective, "&prodID=3")
      ),
      NA_character_
    ),
    cnrfc_release_url = dplyr::if_else(
      .data$has_cnrfc_release,
      dplyr::coalesce(
        pt_lookup_cnrfc_availability(.data$cnrfc_release_id_effective, "cnrfc_release_url"),
        paste0("https://www.cnrfc.noaa.gov/reservoirRelease.php?id=", .data$cnrfc_release_id_effective)
      ),
      NA_character_
    ),
    ## CDEC labels many service times as PST, but the latest-storage web table
    ## displays Pacific clock time.  Use America/Los_Angeles so daylight-saving
    ## dates are not shifted one hour into the future.
    obs_datetime_pacific = dplyr::if_else(
      !is.na(.data$obs_datetime_cdec_display),
      lubridate::mdy_hm(.data$obs_datetime_cdec_display, tz = "America/Los_Angeles"),
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = "America/Los_Angeles")
    ),
    obs_datetime_utc = dplyr::if_else(
      !is.na(.data$obs_datetime_pacific),
      format(lubridate::with_tz(.data$obs_datetime_pacific, "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      NA_character_
    ),
    obs_age_hours = as.numeric(difftime(Sys.time(), .data$obs_datetime_pacific, units = "hours")),
    ## Prefer station-index capacity if it exists; otherwise use the RES daily
    ## capacity field; otherwise use the reviewed static capacity override table.
    ## Capacity is stable metadata, not a live observation.
    capacity_af = dplyr::coalesce(
      .data$capacity_af,
      .data$capacity_af_res,
      .data$capacity_af_override
    ),
    capacity_source_display = dplyr::case_when(
      !is.na(.data$capacity_af_res) & .data$capacity_af == .data$capacity_af_res ~ "CDEC RES daily report",
      !is.na(.data$capacity_af_override) & .data$capacity_af == .data$capacity_af_override ~ .data$capacity_source_name,
      !is.na(.data$capacity_af) ~ "BRIM station index",
      TRUE ~ NA_character_
    ),
    pct_capacity = dplyr::if_else(
      !is.na(.data$capacity_af) & .data$capacity_af > 0 & !is.na(.data$storage_af),
      100 * .data$storage_af / .data$capacity_af,
      NA_real_
    ),
    feed_build_time_utc = feed_build_time_utc,
    feed_source = "CDEC latest sensor-15 table with CDEC RES daily midnight context",
    source_url = cdec_getall_url,
    obs_stale_12h = !is.na(.data$obs_age_hours) & .data$obs_age_hours > 12,
    obs_stale_24h = !is.na(.data$obs_age_hours) & .data$obs_age_hours > 24,
    data_quality_note_live = dplyr::case_when(
      !.data$has_latest_storage & .data$has_midnight_storage ~
        "Latest CDEC sensor-15 storage is not present; feature is shown using CDEC RES daily midnight storage.",
      .data$obs_stale_24h ~
        "CDEC latest storage observation is older than 24 hours; review before use.",
      .data$obs_stale_12h ~
        "CDEC latest storage observation is older than 12 hours; use caution.",
      TRUE ~
        "CDEC provisional latest storage observation."
    )
  ) |>
  dplyr::select(
    dplyr::any_of(c(
      "cdec_id",
      "reservoir_name",
      "cdec_station_name",
      "cdec_station_name_latest",
      "storage_af",
      "storage_maf",
      "display_storage_af",
      "display_storage_maf",
      "display_storage_source",
      "has_latest_storage",
      "has_midnight_storage",
      "capacity_af",
      "capacity_source_display",
      "capacity_source_name",
      "capacity_source_url",
      "capacity_source_date_accessed",
      "capacity_confidence",
      "capacity_note",
      "pct_capacity",
      "obs_datetime_cdec_display",
      "obs_datetime_utc",
      "obs_age_hours",
      "obs_stale_12h",
      "obs_stale_24h",
      "midnight_station_name",
      "midnight_elevation_ft",
      "midnight_storage_af",
      "midnight_storage_maf",
      "midnight_storage_change_af",
      "midnight_pct_capacity",
      "midnight_avg_storage_af",
      "midnight_pct_average",
      "midnight_outflow_cfs",
      "midnight_inflow_cfs",
      "midnight_storage_year_ago_af",
      "midnight_basin_res_table",
      "midnight_report_source_url",
      "midnight_data_status",
      "latitude",
      "longitude",
      "elevation_ft",
      "county",
      "operator_agency",
      "river_basin_cdec",
      "cdec_basin_latest_table",
      "has_hourly_reservoir_report",
      "has_daily_reservoir_report",
      "cdec_station_url",
      "cdec_sensor15_hourly_url",
      "cdec_sensor15_daily_url",
      "cdec_latest_storage_table_url",
      "nws_id",
      "cnrfc_nws_id",
      "cnrfc_obs_id_effective",
      "cnrfc_inflow_id_effective",
      "cnrfc_ensemble_id_effective",
      "cnrfc_release_id_effective",
      "cnrfc_role_override_note",
      "cnrfc_match_confidence",
      "cnrfc_match_method",
      "has_cnrfc_inflow",
      "has_cnrfc_ensemble",
      "has_cnrfc_release",
      "cnrfc_obs_url",
      "cnrfc_inflow_url",
      "cnrfc_ensemble_url",
      "cnrfc_release_url",
      "usace_project_display",
      "usace_california_plots_url",
      "alias_names",
      "feed_build_time_utc",
      "feed_source",
      "source_url",
      "data_quality_note",
      "data_quality_note_live"
    ))
  ) |>
  dplyr::arrange(dplyr::desc(.data$display_storage_af), .data$cdec_id)

latest_count <- sum(!is.na(latest_tbl$storage_af))
midnight_count <- sum(!is.na(latest_tbl$midnight_storage_af))
midnight_only_count <- sum(is.na(latest_tbl$storage_af) & !is.na(latest_tbl$midnight_storage_af))

coverage_status <- dplyr::case_when(
  latest_count == 0 && midnight_count > 0 ~ "daily_fallback_only",
  latest_count < 60 ~ "partial_latest",
  TRUE ~ "normal"
)

coverage_note <- dplyr::case_when(
  coverage_status == "daily_fallback_only" ~
    "CDEC latest sensor-15 table returned no usable current storage rows; feed is using RES daily midnight storage only.",
  coverage_status == "partial_latest" ~
    paste0(
      "CDEC latest sensor-15 table currently contains fewer latest storage rows than usual (",
      latest_count,
      " latest rows). Daily midnight fields are included where available."
    ),
  TRUE ~
    "CDEC latest sensor-15 table coverage appears normal; daily midnight fields are included where available."
)

## Fail-safe publish guard -----------------------------------------------------
##
## CDEC's latest sensor-15 table can intermittently return a very thin subset of
## reservoirs.  When running in GitHub Actions, failing here is intentional: the
## workflow stops before write/commit, so GitHub Pages keeps serving the prior
## good GeoJSON instead of replacing it with a mostly empty layer.
if (latest_count < min_latest_rows_to_publish && !isTRUE(allow_degraded_publish)) {
  stop(
    "CDEC latest sensor-15 coverage is too thin to publish safely: ",
    latest_count,
    " latest-storage features, threshold = ",
    min_latest_rows_to_publish,
    ". Refusing to overwrite the previous feed. ",
    "Set CDEC_ALLOW_DEGRADED_PUBLISH=true only for deliberate debugging."
  )
}

# ---- 7. Write GeoJSON and summary ------------------------------------------

features <- purrr::map(seq_len(nrow(latest_tbl)), ~ pt_make_feature(latest_tbl[.x, , drop = FALSE]))

geojson <- list(
  type = "FeatureCollection",
  name = "cdec_reservoir_latest_storage",
  feed_build_time_utc = feed_build_time_utc,
  source = cdec_getall_url,
  daily_source = cdec_res_daily_url,
  provisional_note = "CDEC provisional data, subject to change.",
  coverage_status = coverage_status,
  coverage_note = coverage_note,
  feature_count = length(features),
  features = features
)

summary_obj <- list(
  feed_build_time_utc = feed_build_time_utc,
  source = cdec_getall_url,
  daily_source = cdec_res_daily_url,
  station_index_rows = nrow(station_index),
  cdec_storage_rows_parsed = nrow(storage_tbl),
  cdec_storage_rows_with_storage = sum(!is.na(storage_tbl$storage_af)),
  cdec_res_daily_rows_parsed = nrow(midnight_tbl),
  cdec_res_daily_rows_with_midnight_storage = sum(!is.na(midnight_tbl$midnight_storage_af)),
  output_feature_count = nrow(latest_tbl),
  output_features_with_latest_storage = latest_count,
  output_features_with_midnight_storage = midnight_count,
  output_features_midnight_only = midnight_only_count,
  coverage_status = coverage_status,
  coverage_note = coverage_note,
  min_latest_rows_to_publish = min_latest_rows_to_publish,
  allow_degraded_publish = allow_degraded_publish,
  max_obs_age_hours = if (latest_count > 0) max(latest_tbl$obs_age_hours[!is.na(latest_tbl$obs_age_hours)], na.rm = TRUE) else NA_real_,
  stale_12h_count = sum(latest_tbl$obs_stale_12h, na.rm = TRUE),
  stale_24h_count = sum(latest_tbl$obs_stale_24h, na.rm = TRUE),
  cnrfc_availability_csv = cnrfc_availability_csv,
  cnrfc_availability_rows = nrow(cnrfc_availability_tbl),
  cnrfc_role_override_csv = cnrfc_role_overrides_csv,
  cnrfc_role_override_rows = nrow(role_overrides_tbl),
  output_features_with_cnrfc_role_override = sum(!is.na(latest_tbl$cnrfc_role_override_note) & latest_tbl$cnrfc_role_override_note != "", na.rm = TRUE),
  capacity_override_csv = capacity_overrides_csv,
  capacity_override_rows = nrow(capacity_overrides_tbl),
  output_features_using_capacity_override = sum(!is.na(latest_tbl$capacity_source_name) & latest_tbl$capacity_source_name != "", na.rm = TRUE),
  output_features_with_capacity = sum(!is.na(latest_tbl$capacity_af) & latest_tbl$capacity_af > 0, na.rm = TRUE),
  cnrfc_inflow_ids_available = sum(cnrfc_availability_tbl$has_cnrfc_inflow, na.rm = TRUE),
  cnrfc_ensemble_ids_available = sum(cnrfc_availability_tbl$has_cnrfc_ensemble, na.rm = TRUE),
  cnrfc_release_ids_available = sum(cnrfc_availability_tbl$has_cnrfc_release, na.rm = TRUE),
  output_features_with_cnrfc_obs = sum(!is.na(latest_tbl$cnrfc_obs_url), na.rm = TRUE),
  output_features_with_cnrfc_inflow = sum(latest_tbl$has_cnrfc_inflow, na.rm = TRUE),
  output_features_with_cnrfc_ensemble = sum(latest_tbl$has_cnrfc_ensemble, na.rm = TRUE),
  output_features_with_cnrfc_release = sum(latest_tbl$has_cnrfc_release, na.rm = TRUE),
  output_features_with_usace_plot_link = sum(!is.na(latest_tbl$usace_california_plots_url), na.rm = TRUE)
)

dir.create(dirname(out_geojson), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_summary), recursive = TRUE, showWarnings = FALSE)

jsonlite::write_json(
  geojson,
  path = out_geojson,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  pretty = FALSE
)

jsonlite::write_json(
  summary_obj,
  path = out_summary,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  pretty = TRUE
)

message("\nDone: CDEC latest reservoir GeoJSON built.")
message("Features: ", nrow(latest_tbl))
message("GeoJSON:  ", out_geojson)
message("Summary:  ", out_summary)
message("Latest storage features: ", summary_obj$output_features_with_latest_storage)
message("Midnight storage features: ", summary_obj$output_features_with_midnight_storage)
message("Midnight-only features: ", summary_obj$output_features_midnight_only)
message("Coverage status: ", summary_obj$coverage_status)
message("Coverage note: ", summary_obj$coverage_note)
message("Minimum latest rows to publish: ", summary_obj$min_latest_rows_to_publish)
message("Allow degraded publish: ", summary_obj$allow_degraded_publish)
message("Max obs age hours: ", round(summary_obj$max_obs_age_hours, 2))
message("Stale >12h: ", summary_obj$stale_12h_count)
message("Stale >24h: ", summary_obj$stale_24h_count)
message("CNRFC availability rows: ", summary_obj$cnrfc_availability_rows)
message("CNRFC role override rows: ", summary_obj$cnrfc_role_override_rows)
message("Features using CNRFC role overrides: ", summary_obj$output_features_with_cnrfc_role_override)
message("CDEC capacity override rows: ", summary_obj$capacity_override_rows)
message("Features using capacity overrides: ", summary_obj$output_features_using_capacity_override)
message("Features with capacity: ", summary_obj$output_features_with_capacity)
message("CNRFC inflow IDs available: ", summary_obj$cnrfc_inflow_ids_available)
message("CNRFC ensemble IDs available: ", summary_obj$cnrfc_ensemble_ids_available)
message("CNRFC release IDs available: ", summary_obj$cnrfc_release_ids_available)
message("Features with CNRFC observed reservoir links: ", summary_obj$output_features_with_cnrfc_obs)
message("Features with CNRFC inflow links: ", summary_obj$output_features_with_cnrfc_inflow)
message("Features with CNRFC ensemble links: ", summary_obj$output_features_with_cnrfc_ensemble)
message("Features with CNRFC release links: ", summary_obj$output_features_with_cnrfc_release)
message("Features with USACE plot links: ", summary_obj$output_features_with_usace_plot_link)
