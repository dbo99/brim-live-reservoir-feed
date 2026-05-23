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

cnrfc_inflow_list_url <- Sys.getenv(
  "CNRFC_RSVR_INFLOW_LIST_URL",
  unset = "https://www.cnrfc.noaa.gov/?product=rsvrInflow"
)

cnrfc_release_list_url <- Sys.getenv(
  "CNRFC_RSVR_RELEASE_LIST_URL",
  unset = "https://www.cnrfc.noaa.gov/?product=rsvrRelease"
)

usace_ca_plots_url <- Sys.getenv(
  "USACE_CA_RESERVOIR_PLOTS_URL",
  unset = "https://www.spk-wc.usace.army.mil/plots/california.html"
)

feed_build_time_utc <- format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# ---- 3. Helpers -------------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
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
      rawToChar(raw)
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
  ## option values, JavaScript, or visible text.  Extract broadly, then intersect
  ## with known BRIM CNRFC/NWS IDs so unrelated page text cannot create popup
  ## links.
  known_ids <- toupper(pt_chr(known_ids))
  known_ids <- known_ids[!is.na(known_ids) & known_ids != ""]

  if (length(known_ids) == 0 || is.null(html) || !nzchar(html)) {
    return(character())
  }

  txt <- toupper(html)

  id_from_params <- stringr::str_match_all(
    txt,
    "(?:[?&]ID=|\\bID=|VALUE=['\\\"]?)([A-Z0-9]{3,6})"
  )[[1]]

  ids1 <- if (nrow(id_from_params) > 0) id_from_params[, 2] else character()

  ids2 <- stringr::str_extract_all(
    txt,
    "\\b[A-Z0-9]{3,6}[A-Z][0-9]\\b"
  )[[1]]

  ids <- sort(unique(c(ids1, ids2)))
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

# ---- 5. Fetch and parse current storage ------------------------------------

storage_html <- pt_fetch_text(cdec_getall_url, label = "CDEC latest reservoir storage table")
storage_tbl <- pt_parse_getall_storage(storage_html)

message("CDEC latest storage rows parsed: ", nrow(storage_tbl))
message("CDEC latest storage rows with numeric storage_af: ", sum(!is.na(storage_tbl$storage_af)))

if (nrow(storage_tbl) > 0 && all(is.na(storage_tbl$storage_af))) {
  stop(
    "CDEC storage rows were parsed, but storage_af is NA for every row. ",
    "This usually means CDEC changed the value-cell text or unit formatting. ",
    "Example value cells should be inspected before writing an empty GeoJSON."
  )
}

# ---- 5.1 Fetch CNRFC reservoir-product availability -------------------------
#
# These product lists determine whether a live CDEC reservoir popup should show
# a CNRFC reservoir-inflow or reservoir-release link.  If CNRFC changes the
# products or adds reservoirs, the next scheduled feed build can pick that up
# without requiring a BRIM HTML rebuild.

known_cnrfc_ids <- sort(unique(pt_chr(station_index$cnrfc_nws_id)))

cnrfc_inflow_ids <- pt_fetch_cnrfc_product_ids(
  url = cnrfc_inflow_list_url,
  label = "CNRFC reservoir inflow product list",
  known_ids = known_cnrfc_ids
)

cnrfc_release_ids <- pt_fetch_cnrfc_product_ids(
  url = cnrfc_release_list_url,
  label = "CNRFC reservoir release product list",
  known_ids = known_cnrfc_ids
)

usace_ca_lookup <- pt_usace_ca_reservoir_lookup(usace_ca_plots_url)

# ---- 6. Join storage to station index --------------------------------------

latest_tbl <- station_index |>
  dplyr::left_join(storage_tbl, by = "cdec_id") |>
  dplyr::left_join(usace_ca_lookup, by = "cdec_id") |>
  dplyr::filter(!is.na(.data$storage_af)) |>
  dplyr::mutate(
    storage_maf = .data$storage_af / 1e6,
    has_cnrfc_inflow = !is.na(.data$cnrfc_nws_id) & .data$cnrfc_nws_id %in% cnrfc_inflow_ids,
    has_cnrfc_release = !is.na(.data$cnrfc_nws_id) & .data$cnrfc_nws_id %in% cnrfc_release_ids,
    ## CNRFC reservoir-link conventions:
    ##   - obsRiver_hc.php is the observed river/reservoir conditions page and
    ##     can show reservoir elevation/storage detail for reservoir points.
    ##   - reservoir.php is the reservoir-inflow / graphical RVF page.
    ##   - ensembleProduct.php?prodID=3 is the HEFS/ensemble forecast page and
    ##     is shown only where the current CNRFC inflow product list includes
    ##     the NWS/CNRFC ID.
    ##   - reservoirRelease.php is the reservoir-release page and is shown only
    ##     where the current CNRFC release product list includes the NWS/CNRFC ID.
    cnrfc_obs_url = dplyr::if_else(
      !is.na(.data$cnrfc_nws_id) & .data$cnrfc_nws_id != "",
      paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", .data$cnrfc_nws_id),
      NA_character_
    ),
    cnrfc_inflow_url = dplyr::if_else(
      .data$has_cnrfc_inflow,
      paste0("https://www.cnrfc.noaa.gov/reservoir.php?id=", .data$cnrfc_nws_id),
      NA_character_
    ),
    cnrfc_ensemble_url = dplyr::if_else(
      .data$has_cnrfc_inflow,
      paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", .data$cnrfc_nws_id, "&prodID=3"),
      NA_character_
    ),
    cnrfc_release_url = dplyr::if_else(
      .data$has_cnrfc_release,
      paste0("https://www.cnrfc.noaa.gov/reservoirRelease.php?id=", .data$cnrfc_nws_id),
      NA_character_
    ),
    ## CDEC labels many service times as PST, but the latest-storage web table
    ## displays Pacific clock time.  Use America/Los_Angeles so daylight-saving
    ## dates are not shifted one hour into the future.
    obs_datetime_pacific = lubridate::mdy_hm(.data$obs_datetime_cdec_display, tz = "America/Los_Angeles"),
    obs_datetime_utc = format(lubridate::with_tz(.data$obs_datetime_pacific, "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    obs_age_hours = as.numeric(difftime(Sys.time(), .data$obs_datetime_pacific, units = "hours")),
    pct_capacity = dplyr::if_else(!is.na(.data$capacity_af) & .data$capacity_af > 0, 100 * .data$storage_af / .data$capacity_af, NA_real_),
    feed_build_time_utc = feed_build_time_utc,
    feed_source = "CDEC getAll sensor 15 latest reservoir storage table",
    source_url = cdec_getall_url,
    obs_stale_12h = .data$obs_age_hours > 12,
    obs_stale_24h = .data$obs_age_hours > 24,
    data_quality_note_live = dplyr::case_when(
      .data$obs_stale_24h ~ "CDEC latest storage observation is older than 24 hours; review before use.",
      .data$obs_stale_12h ~ "CDEC latest storage observation is older than 12 hours; use caution.",
      TRUE ~ "CDEC provisional latest storage observation."
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
      "capacity_af",
      "pct_capacity",
      "obs_datetime_cdec_display",
      "obs_datetime_utc",
      "obs_age_hours",
      "obs_stale_12h",
      "obs_stale_24h",
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
      "cnrfc_match_confidence",
      "cnrfc_match_method",
      "has_cnrfc_inflow",
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
  dplyr::arrange(dplyr::desc(.data$storage_af), .data$cdec_id)

# ---- 7. Write GeoJSON and summary ------------------------------------------

features <- purrr::map(seq_len(nrow(latest_tbl)), ~ pt_make_feature(latest_tbl[.x, , drop = FALSE]))

geojson <- list(
  type = "FeatureCollection",
  name = "cdec_reservoir_latest_storage",
  feed_build_time_utc = feed_build_time_utc,
  source = cdec_getall_url,
  provisional_note = "CDEC provisional data, subject to change.",
  feature_count = length(features),
  features = features
)

summary_obj <- list(
  feed_build_time_utc = feed_build_time_utc,
  source = cdec_getall_url,
  station_index_rows = nrow(station_index),
  cdec_storage_rows_parsed = nrow(storage_tbl),
  output_feature_count = nrow(latest_tbl),
  max_obs_age_hours = if (nrow(latest_tbl) > 0) max(latest_tbl$obs_age_hours, na.rm = TRUE) else NA_real_,
  stale_12h_count = sum(latest_tbl$obs_stale_12h, na.rm = TRUE),
  stale_24h_count = sum(latest_tbl$obs_stale_24h, na.rm = TRUE),
  cnrfc_inflow_ids_detected = length(cnrfc_inflow_ids),
  cnrfc_release_ids_detected = length(cnrfc_release_ids),
  output_features_with_cnrfc_obs = sum(!is.na(latest_tbl$cnrfc_obs_url), na.rm = TRUE),
  output_features_with_cnrfc_inflow = sum(latest_tbl$has_cnrfc_inflow, na.rm = TRUE),
  output_features_with_cnrfc_ensemble = sum(!is.na(latest_tbl$cnrfc_ensemble_url), na.rm = TRUE),
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
message("Max obs age hours: ", round(summary_obj$max_obs_age_hours, 2))
message("Stale >12h: ", summary_obj$stale_12h_count)
message("Stale >24h: ", summary_obj$stale_24h_count)
message("Features with CNRFC observed reservoir links: ", summary_obj$output_features_with_cnrfc_obs)
message("Features with CNRFC inflow links: ", summary_obj$output_features_with_cnrfc_inflow)
message("Features with CNRFC ensemble links: ", summary_obj$output_features_with_cnrfc_ensemble)
message("Features with CNRFC release links: ", summary_obj$output_features_with_cnrfc_release)
message("Features with USACE plot links: ", summary_obj$output_features_with_usace_plot_link)
