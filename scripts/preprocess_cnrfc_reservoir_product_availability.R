# ==== preprocess_cnrfc_reservoir_product_availability.R =======================
#
# PURPOSE:
#   Build a stable CNRFC reservoir-product availability table for BRIM live
#   reservoir-storage popups.
#
# OUTPUT:
#   data/input/cnrfc_reservoir_product_availability.csv
#   data/input/cnrfc_reservoir_product_availability_summary.json
#
# DESIGN:
#   - Product availability is metadata, not live storage data.
#   - This script is intended to be run manually/occasionally, not every six
#     hours in GitHub Actions.
#   - The scheduled live feed builder reads the CSV produced here and only
#     fetches CDEC storage values.
#
# RECOMMENDED REFRESH TIMING:
#   - Early water year / after CNRFC water-year rollover.
#   - Mid/late summer if CNRFC product development is expected.
#   - Any time BRIM shows a missing or extra CNRFC reservoir product link.
# ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c("curl", "dplyr", "readr", "stringr", "jsonlite", "tibble", "purrr")

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running this preprocessor."
  )
}

suppressPackageStartupMessages({
  library(curl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(jsonlite)
  library(tibble)
  library(purrr)
})

# ---- 2. Paths and constants -------------------------------------------------

station_index_csv <- Sys.getenv(
  "CDEC_STATION_INDEX_CSV",
  unset = "data/input/cdec_reservoir_station_index.csv"
)

out_csv <- Sys.getenv(
  "CNRFC_RESERVOIR_PRODUCT_AVAILABILITY_CSV",
  unset = "data/input/cnrfc_reservoir_product_availability.csv"
)

out_summary <- Sys.getenv(
  "CNRFC_RESERVOIR_PRODUCT_AVAILABILITY_SUMMARY_JSON",
  unset = "data/input/cnrfc_reservoir_product_availability_summary.json"
)

cnrfc_inflow_list_url <- Sys.getenv(
  "CNRFC_RSVR_INFLOW_LIST_URL",
  unset = "https://www.cnrfc.noaa.gov/reservoir.php?id=ANTC1"
)

cnrfc_release_list_url <- Sys.getenv(
  "CNRFC_RSVR_RELEASE_LIST_URL",
  unset = "https://www.cnrfc.noaa.gov/reservoirRelease.php?id=KWKC1"
)

cnrfc_ensemble_list_url <- Sys.getenv(
  "CNRFC_ENSEMBLE_LIST_URL",
  unset = "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=ANTC1&prodID=3"
)

availability_build_time_utc <- format(
  as.POSIXct(Sys.time(), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)

# ---- 3. Helpers -------------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_force_utf8 <- function(x) {
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
    "BRIM CNRFC product availability preprocessor",
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

pt_visible_text <- function(html) {
  txt <- pt_force_utf8(html)
  txt <- gsub("(?is)<script[^>]*>.*?</script>", " ", txt, perl = TRUE)
  txt <- gsub("(?is)<style[^>]*>.*?</style>", " ", txt, perl = TRUE)
  txt <- gsub("(?i)<br\\s*/?>", " ", txt, perl = TRUE)
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("(?i)&nbsp;?", " ", txt, perl = TRUE)
  txt <- gsub("(?i)&(ensp|emsp);?", " ", txt, perl = TRUE)
  txt <- gsub("&#160;", " ", txt, fixed = TRUE)
  txt <- gsub("&#xa0;", " ", txt, ignore.case = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
  txt <- gsub("&#39;|&apos;", "'", txt, ignore.case = TRUE)
  txt <- gsub("\\s+", " ", txt)
  toupper(trimws(txt))
}

pt_location_block <- function(html) {
  txt <- pt_visible_text(html)

  loc_start <- regexpr("\\bLOCATION\\s*:", txt, perl = TRUE)

  if (is.na(loc_start[1]) || loc_start[1] <= 0) {
    return(txt)
  }

  after_loc <- substring(txt, loc_start[1] + attr(loc_start, "match.length"))
  loc_end <- regexpr("\\bLATITUDE\\s*:", after_loc, perl = TRUE)

  if (!is.na(loc_end[1]) && loc_end[1] > 0) {
    return(substring(after_loc, 1, loc_end[1] - 1))
  }

  ## Fallback to a bounded slice after Location if CNRFC changes the page
  ## structure.  This is conservative compared to scanning the entire page.
  substring(after_loc, 1, min(nchar(after_loc), 12000))
}

pt_extract_product_ids <- function(html, known_ids) {
  known_ids <- sort(unique(toupper(pt_chr(known_ids))))
  known_ids <- known_ids[!is.na(known_ids) & known_ids != ""]

  if (length(known_ids) == 0 || is.null(html) || !nzchar(html)) {
    return(character())
  }

  product_txt <- pt_location_block(html)

  ids0 <- known_ids[
    vapply(
      known_ids,
      function(id) grepl(paste0("\\b", id, "\\b"), product_txt, perl = TRUE),
      logical(1)
    )
  ]

  id_from_params <- stringr::str_match_all(
    product_txt,
    "(?:[?&]ID=|\\bID\\s*[=:]\\s*['\\\"]?|VALUE\\s*=\\s*['\\\"]?)([A-Z0-9]{3,8})"
  )[[1]]

  ids1 <- if (nrow(id_from_params) > 0) id_from_params[, 2] else character()

  ids2 <- stringr::str_extract_all(
    product_txt,
    "\\b[A-Z0-9]{3,6}[A-Z][0-9]\\b"
  )[[1]]

  ids <- sort(unique(c(ids0, ids1, ids2)))
  ids <- ids[!is.na(ids) & ids != ""]
  intersect(ids, known_ids)
}

# ---- 4. Read BRIM station index --------------------------------------------

if (!file.exists(station_index_csv)) {
  stop(
    "Station index CSV not found: ", station_index_csv,
    "\nRun scripts/export_brim_cdec_station_index.R from the BRIM project first."
  )
}

station_index_raw <- readr::read_csv(
  station_index_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

if (!"cnrfc_nws_id" %in% names(station_index_raw)) {
  station_index_raw$cnrfc_nws_id <- NA_character_
}

if (!"nws_id" %in% names(station_index_raw)) {
  station_index_raw$nws_id <- NA_character_
}

if (!"cdec_id" %in% names(station_index_raw)) {
  station_index_raw$cdec_id <- NA_character_
}

station_xwalk <- station_index_raw |>
  dplyr::mutate(
    cdec_id = toupper(pt_chr(.data$cdec_id)),
    cnrfc_nws_id = dplyr::coalesce(
      toupper(pt_chr(.data$cnrfc_nws_id)),
      toupper(pt_chr(.data$nws_id))
    )
  ) |>
  dplyr::filter(!is.na(.data$cnrfc_nws_id), .data$cnrfc_nws_id != "")

known_cnrfc_ids <- sort(unique(station_xwalk$cnrfc_nws_id))

message("Known CNRFC/NWS IDs from BRIM station index: ", length(known_cnrfc_ids))

if (length(known_cnrfc_ids) == 0) {
  stop("No CNRFC/NWS IDs found in station index; cannot build availability table.")
}

cdec_id_lookup <- station_xwalk |>
  dplyr::group_by(.data$cnrfc_nws_id) |>
  dplyr::summarise(
    cdec_ids = paste(sort(unique(.data$cdec_id[!is.na(.data$cdec_id)])), collapse = ";"),
    .groups = "drop"
  )

# ---- 5. Fetch CNRFC product pages ------------------------------------------

inflow_html <- pt_fetch_text(cnrfc_inflow_list_url, label = "CNRFC reservoir inflow page")
ensemble_html <- pt_fetch_text(cnrfc_ensemble_list_url, label = "CNRFC ensemble product page")
release_html <- pt_fetch_text(cnrfc_release_list_url, label = "CNRFC reservoir release schedule page")

inflow_ids <- pt_extract_product_ids(inflow_html, known_cnrfc_ids)
ensemble_ids <- pt_extract_product_ids(ensemble_html, known_cnrfc_ids)
release_ids <- pt_extract_product_ids(release_html, known_cnrfc_ids)

message("CNRFC inflow IDs available: ", length(inflow_ids))
message("CNRFC ensemble IDs available: ", length(ensemble_ids))
message("CNRFC release IDs available: ", length(release_ids))

# ---- 6. Build availability table -------------------------------------------

availability_tbl <- tibble::tibble(
  cnrfc_nws_id = known_cnrfc_ids
) |>
  dplyr::left_join(cdec_id_lookup, by = "cnrfc_nws_id") |>
  dplyr::mutate(
    has_cnrfc_inflow = .data$cnrfc_nws_id %in% inflow_ids,
    has_cnrfc_ensemble = .data$cnrfc_nws_id %in% ensemble_ids,
    has_cnrfc_release = .data$cnrfc_nws_id %in% release_ids,
    cnrfc_obs_url = paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", .data$cnrfc_nws_id),
    cnrfc_inflow_url = dplyr::if_else(
      .data$has_cnrfc_inflow,
      paste0("https://www.cnrfc.noaa.gov/reservoir.php?id=", .data$cnrfc_nws_id),
      NA_character_
    ),
    cnrfc_ensemble_url = dplyr::if_else(
      .data$has_cnrfc_ensemble,
      paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", .data$cnrfc_nws_id, "&prodID=3"),
      NA_character_
    ),
    cnrfc_release_url = dplyr::if_else(
      .data$has_cnrfc_release,
      paste0("https://www.cnrfc.noaa.gov/reservoirRelease.php?id=", .data$cnrfc_nws_id),
      NA_character_
    ),
    cnrfc_inflow_source_url = cnrfc_inflow_list_url,
    cnrfc_ensemble_source_url = cnrfc_ensemble_list_url,
    cnrfc_release_source_url = cnrfc_release_list_url,
    availability_build_time_utc = availability_build_time_utc,
    availability_parser_note = "IDs extracted from visible CNRFC Location block and intersected with BRIM CDEC-CNRFC station index."
  ) |>
  dplyr::arrange(.data$cnrfc_nws_id)

summary_obj <- list(
  availability_build_time_utc = availability_build_time_utc,
  station_index_csv = station_index_csv,
  output_csv = out_csv,
  known_cnrfc_ids = length(known_cnrfc_ids),
  inflow_source = cnrfc_inflow_list_url,
  ensemble_source = cnrfc_ensemble_list_url,
  release_source = cnrfc_release_list_url,
  cnrfc_inflow_ids_available = length(inflow_ids),
  cnrfc_ensemble_ids_available = length(ensemble_ids),
  cnrfc_release_ids_available = length(release_ids),
  rows_written = nrow(availability_tbl)
)

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_summary), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(availability_tbl, out_csv, na = "")

jsonlite::write_json(
  summary_obj,
  path = out_summary,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  pretty = TRUE
)

message("\nDone: CNRFC reservoir product availability table built.")
message("CSV:     ", out_csv)
message("Summary: ", out_summary)
message("Rows:    ", nrow(availability_tbl))
message("Inflow:  ", length(inflow_ids))
message("Ensemble:", length(ensemble_ids))
message("Release: ", length(release_ids))
