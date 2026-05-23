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
#   CDEC report times are treated as Pacific Standard Time, matching CDEC's
#   documentation language for web-service output.  The raw display string is
#   preserved as obs_datetime_cdec_display.
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

station_index <- station_index_raw |>
  dplyr::mutate(
    cdec_id = toupper(trimws(as.character(.data$cdec_id))),
    latitude = pt_num(.data$latitude),
    longitude = pt_num(.data$longitude),
    elevation_ft = pt_num(.data$elevation_ft),
    capacity_af = if ("capacity_af" %in% names(station_index_raw)) pt_num(station_index_raw$capacity_af) else NA_real_
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

# ---- 6. Join storage to station index --------------------------------------

latest_tbl <- station_index |>
  dplyr::left_join(storage_tbl, by = "cdec_id") |>
  dplyr::filter(!is.na(.data$storage_af)) |>
  dplyr::mutate(
    storage_maf = .data$storage_af / 1e6,
    obs_datetime_pst = lubridate::mdy_hm(.data$obs_datetime_cdec_display, tz = "Etc/GMT+8"),
    obs_datetime_utc = format(lubridate::with_tz(.data$obs_datetime_pst, "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    obs_age_hours = as.numeric(difftime(Sys.time(), .data$obs_datetime_pst, units = "hours")),
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
  stale_24h_count = sum(latest_tbl$obs_stale_24h, na.rm = TRUE)
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
