# ==== build_usgs_groundwater_latest_ca.R =====================================
#
# PURPOSE:
#   Fetch latest/recent USGS California groundwater-level field measurements
#   for the BRIM Ops Live groundwater layer and write a small static GeoJSON
#   feed suitable for GitHub Pages.
#
# OUTPUTS:
#   docs/data/usgs_groundwater_latest_ca.geojson
#   docs/data/usgs_groundwater_latest_ca_summary.json
#
# DESIGN:
#   - The station/candidate index is committed to the live-data feed repo.
#   - Latest groundwater levels come from the modern USGS Water Data API
#     field-measurements endpoint through dataRetrieval.
#   - The feed uses parameter 72019, depth to water level in feet below land
#     surface / below ground surface, because that is the value BRIM needs for
#     quick screening popups.
#   - The input index remains the stable backbone for locations and well
#     construction/aquifer fields; API field measurements are refreshed daily.
#
# HOW TO RUN LOCALLY FROM THE FEED REPOSITORY:
#   Rscript scripts/build_usgs_groundwater_latest_ca.R
#
# REQUIRED INPUT:
#   data/input/usgs_groundwater_latest_index_ca.csv
#
# NOTES:
#   USGS groundwater field measurements are low-frequency, site-visit data and
#   may be provisional. They are appropriate for screening/situational awareness,
#   not for final hydrogeologic conclusions without review of well construction,
#   datum, aquifer/screen interval, and measurement history.
# ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c(
  "dataRetrieval", "dplyr", "readr", "lubridate", "jsonlite", "tibble", "curl"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running locally, or let the GitHub Action install them."
  )
}

required_dataretrieval_funs <- c("read_waterdata_field_measurements")

missing_dataretrieval_funs <- required_dataretrieval_funs[
  !vapply(required_dataretrieval_funs, exists, logical(1), where = asNamespace("dataRetrieval"), inherits = FALSE)
]

if (length(missing_dataretrieval_funs) > 0) {
  stop(
    "Installed dataRetrieval package is too old for RF027b. Missing function(s): ",
    paste(missing_dataretrieval_funs, collapse = ", "),
    "\nUpdate dataRetrieval from CRAN, then rerun."
  )
}

## Suppress package progress UI; this feed prints its own compact progress.
options(
  cli.progress_show_after = Inf,
  cli.progress_handlers = "none"
)

suppressPackageStartupMessages({
  library(dataRetrieval)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(jsonlite)
  library(tibble)
  library(curl)
})

# ---- 2. Paths, constants, and switches -------------------------------------

station_index_csv <- Sys.getenv(
  "USGS_GW_STATION_INDEX_CSV",
  unset = "data/input/usgs_groundwater_latest_index_ca.csv"
)

## RF029:
##   Optional compact history-summary table produced by the local BRIM-side
##   groundwater history preprocessor. If this CSV is present in the live-feed
##   repo input folder, the daily latest feed joins it into the GeoJSON feature
##   properties. If it is absent, the latest-only feed still runs normally.
history_summary_csv <- Sys.getenv(
  "USGS_GW_HISTORY_SUMMARY_CSV",
  unset = "data/input/usgs_groundwater_history_summary_ca.csv"
)

out_geojson <- Sys.getenv(
  "USGS_GW_GEOJSON",
  unset = "docs/data/usgs_groundwater_latest_ca.geojson"
)

out_summary <- Sys.getenv(
  "USGS_GW_SUMMARY_JSON",
  unset = "docs/data/usgs_groundwater_latest_ca_summary.json"
)

gw_parameter_code <- Sys.getenv(
  "USGS_GW_PARAMETER_CODE",
  unset = "72019"
)

field_measurements_lookback_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_FIELD_MEASUREMENTS_LOOKBACK_DAYS",
  unset = "800"
)))
if (is.na(field_measurements_lookback_days) || field_measurements_lookback_days < 30) {
  field_measurements_lookback_days <- 800L
}

chunk_size <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_CHUNK_SIZE",
  unset = "60"
)))
if (is.na(chunk_size) || chunk_size < 1) chunk_size <- 60L

request_pause_sec <- suppressWarnings(as.numeric(Sys.getenv(
  "USGS_GW_REQUEST_PAUSE_SEC",
  unset = "0.20"
)))
if (is.na(request_pause_sec) || request_pause_sec < 0) request_pause_sec <- 0.20

min_api_sites_to_publish <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_MIN_API_SITES_TO_PUBLISH",
  unset = "300"
)))
if (is.na(min_api_sites_to_publish) || min_api_sites_to_publish < 1) {
  min_api_sites_to_publish <- 300L
}

min_features_to_publish <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_MIN_FEATURES_TO_PUBLISH",
  unset = "300"
)))
if (is.na(min_features_to_publish) || min_features_to_publish < 1) {
  min_features_to_publish <- 300L
}

allow_index_fallback <- tolower(Sys.getenv(
  "USGS_GW_ALLOW_INDEX_FALLBACK",
  unset = "true"
)) %in% c("true", "t", "1", "yes", "y")

feed_build_time <- Sys.time()
feed_build_time_utc <- format(lubridate::with_tz(feed_build_time, "UTC"), "%Y-%m-%dT%H:%M:%SZ")

run_date_utc <- as.Date(lubridate::with_tz(feed_build_time, "UTC"))
query_start_date <- as.character(run_date_utc - field_measurements_lookback_days)
query_end_date <- as.character(run_date_utc + 1)

# ---- 3. Small helpers -------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_site_no <- function(x) {
  x <- pt_chr(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) == 0] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

pt_bool <- function(x) {
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

pt_ensure_cols <- function(df, cols) {
  for (nm in cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
  }
  df
}

pt_chunks <- function(x, n) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & x != ""]
  split(x, ceiling(seq_along(x) / n))
}

pt_usgs_monitoring_location_id <- function(site_ids) {
  site_ids <- pt_site_no(site_ids)
  out <- ifelse(!is.na(site_ids) & site_ids != "", paste0("USGS-", site_ids), NA_character_)
  out[!is.na(out)]
}

pt_waterdata_time_interval <- function(start_date, end_date) {
  paste0(as.character(start_date), "/", as.character(end_date))
}

pt_has_cols <- function(df, cols) {
  all(cols %in% names(df))
}

pt_compact_code <- function(qualifier, approval_status) {
  qualifier <- pt_chr(qualifier)
  approval_status <- pt_chr(approval_status)

  dplyr::case_when(
    !is.na(qualifier) & !is.na(approval_status) ~ paste0(qualifier, "; ", approval_status),
    !is.na(qualifier) ~ qualifier,
    !is.na(approval_status) ~ approval_status,
    TRUE ~ NA_character_
  )
}

pt_depth_class <- function(x) {
  x <- pt_num(x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x < 0 ~ "above land surface / flowing or anomalous",
    x <= 25 ~ "0-25 ft bgs",
    x <= 50 ~ "25-50 ft bgs",
    x <= 100 ~ "50-100 ft bgs",
    x <= 250 ~ "100-250 ft bgs",
    x <= 500 ~ "250-500 ft bgs",
    TRUE ~ ">500 ft bgs"
  )
}

pt_ogc_query_url <- function(collection, params) {
  base <- paste0(
    "https://api.waterdata.usgs.gov/ogcapi/v0/collections/",
    collection,
    "/items"
  )

  params <- params[!vapply(params, function(x) is.null(x) || length(x) == 0, logical(1))]
  params <- lapply(params, function(x) paste(as.character(x), collapse = ","))

  query <- paste(
    paste0(
      names(params),
      "=",
      vapply(params, utils::URLencode, character(1), reserved = TRUE)
    ),
    collapse = "&"
  )

  paste0(base, "?", query)
}

pt_fetch_ogc_properties <- function(url, label = url) {
  message("Requesting direct Water Data API fallback: ", label)

  h <- curl::new_handle(
    timeout = 45,
    connecttimeout = 15,
    useragent = "BRIM live groundwater feed"
  )

  api_key <- Sys.getenv("API_USGS_PAT")
  if (nzchar(api_key)) {
    curl::handle_setheaders(h, "X-Api-Key" = api_key)
  }

  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    warning("Direct Water Data API fallback failed for ", label, ": ", conditionMessage(resp))
    return(tibble::tibble())
  }

  if (!is.null(resp$status_code) && resp$status_code >= 400) {
    warning("Direct Water Data API fallback returned HTTP ", resp$status_code, " for ", label)
    return(tibble::tibble())
  }

  x <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE),
    error = function(e) e
  )

  if (inherits(x, "error")) {
    warning("Could not parse direct Water Data API fallback JSON for ", label, ": ", conditionMessage(x))
    return(tibble::tibble())
  }

  if (!"features" %in% names(x) || is.null(x$features) || length(x$features) == 0) {
    return(tibble::tibble())
  }

  props <- NULL

  if (is.data.frame(x$features) && "properties" %in% names(x$features)) {
    props <- x$features$properties
  } else if (is.list(x$features) && !is.null(x$features$properties)) {
    props <- x$features$properties
  }

  if (is.null(props)) return(tibble::tibble())

  tibble::as_tibble(props)
}

pt_fetch_field_measurements_chunk_direct <- function(site_ids, start_date, end_date, label) {
  url <- pt_ogc_query_url(
    collection = "field-measurements",
    params = list(
      f = "json",
      lang = "en-US",
      skipGeometry = "TRUE",
      properties = paste(
        c(
          "monitoring_location_id",
          "parameter_code",
          "time",
          "value",
          "unit_of_measure",
          "qualifier",
          "approval_status",
          "observing_procedure",
          "vertical_datum",
          "measuring_agency",
          "field_visit_id",
          "last_modified"
        ),
        collapse = ","
      ),
      monitoring_location_id = paste(pt_usgs_monitoring_location_id(site_ids), collapse = ","),
      parameter_code = gw_parameter_code,
      time = pt_waterdata_time_interval(start_date, end_date),
      limit = "50000"
    )
  )

  pt_fetch_ogc_properties(url, label = paste0("groundwater field-measurements chunk ", label))
}

pt_fetch_field_measurements_chunk <- function(site_ids, start_date, end_date, label) {
  tryCatch({
    dataRetrieval::read_waterdata_field_measurements(
      monitoring_location_id = pt_usgs_monitoring_location_id(site_ids),
      parameter_code = gw_parameter_code,
      time = pt_waterdata_time_interval(start_date, end_date),
      properties = c(
        "monitoring_location_id",
        "parameter_code",
        "time",
        "value",
        "unit_of_measure",
        "qualifier",
        "approval_status",
        "observing_procedure",
        "vertical_datum",
        "measuring_agency",
        "field_visit_id",
        "last_modified"
      ),
      skipGeometry = TRUE
    )
  }, error = function(e) {
    warning(
      "USGS groundwater field-measurements chunk failed through dataRetrieval for ",
      label,
      ": ",
      conditionMessage(e),
      ". Trying direct OGC API fallback."
    )
    pt_fetch_field_measurements_chunk_direct(
      site_ids = site_ids,
      start_date = start_date,
      end_date = end_date,
      label = label
    )
  })
}

pt_empty_gw_latest <- function() {
  tibble::tibble(
    site_no = character(),
    api_latest_wl_ft_bgs = numeric(),
    api_latest_wl_datetime_utc = character(),
    api_latest_wl_date = as.Date(character()),
    api_latest_wl_status = character(),
    api_latest_wl_procedure = character(),
    api_latest_wl_qualifier = character(),
    api_latest_wl_units = character(),
    api_latest_wl_vertical_datum = character(),
    api_latest_wl_measuring_agency = character(),
    api_latest_wl_field_visit_id = character(),
    api_latest_wl_last_modified_utc = character()
  )
}

pt_fetch_latest_groundwater <- function(site_ids, start_date, end_date) {
  chunks <- pt_chunks(site_ids, chunk_size)

  message("Fetching USGS groundwater field measurements in ", length(chunks), " chunk(s).")
  message("USGS field-measurements time interval: ", pt_waterdata_time_interval(start_date, end_date))
  message("USGS groundwater parameter code: ", gw_parameter_code)

  out <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    message("  field-measurements chunk ", i, " of ", length(chunks), " | sites: ", length(chunks[[i]]))
    out[[i]] <- pt_fetch_field_measurements_chunk(
      site_ids = chunks[[i]],
      start_date = start_date,
      end_date = end_date,
      label = paste0(i, "/", length(chunks))
    )

    if (i < length(chunks) && request_pause_sec > 0) Sys.sleep(request_pause_sec)
  }

  raw <- dplyr::bind_rows(out)
  message("USGS groundwater field-measurements raw rows fetched: ", nrow(raw))

  if (nrow(raw) == 0) {
    return(list(raw = raw, latest = pt_empty_gw_latest()))
  }

  needed <- c("monitoring_location_id", "parameter_code", "time", "value")
  if (!pt_has_cols(raw, needed)) {
    warning(
      "USGS field-measurements output did not include expected columns: ",
      paste(setdiff(needed, names(raw)), collapse = ", "),
      ". Skipping API latest groundwater values."
    )
    return(list(raw = raw, latest = pt_empty_gw_latest()))
  }

  raw2 <- raw |>
    dplyr::mutate(
      site_no = pt_site_no(.data$monitoring_location_id),
      parameter_code = as.character(.data$parameter_code),
      obs_datetime = suppressWarnings(lubridate::as_datetime(.data$time, tz = "UTC")),
      obs_value = pt_num(.data$value),
      obs_code = pt_compact_code(
        if ("qualifier" %in% names(raw)) .data$qualifier else NA_character_,
        if ("approval_status" %in% names(raw)) .data$approval_status else NA_character_
      ),
      unit_of_measure = if ("unit_of_measure" %in% names(raw)) pt_chr(.data$unit_of_measure) else NA_character_,
      observing_procedure = if ("observing_procedure" %in% names(raw)) pt_chr(.data$observing_procedure) else NA_character_,
      vertical_datum = if ("vertical_datum" %in% names(raw)) pt_chr(.data$vertical_datum) else NA_character_,
      measuring_agency = if ("measuring_agency" %in% names(raw)) pt_chr(.data$measuring_agency) else NA_character_,
      field_visit_id = if ("field_visit_id" %in% names(raw)) pt_chr(.data$field_visit_id) else NA_character_,
      last_modified = if ("last_modified" %in% names(raw)) pt_chr(.data$last_modified) else NA_character_,
      last_modified_utc = suppressWarnings(lubridate::as_datetime(.data$last_modified, tz = "UTC"))
    ) |>
    dplyr::filter(
      !is.na(.data$site_no),
      .data$parameter_code == gw_parameter_code,
      !is.na(.data$obs_datetime),
      !is.na(.data$obs_value)
    )

  if (nrow(raw2) == 0) {
    return(list(raw = raw, latest = pt_empty_gw_latest()))
  }

  latest <- raw2 |>
    dplyr::arrange(.data$site_no, .data$obs_datetime) |>
    dplyr::group_by(.data$site_no) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      api_latest_wl_datetime_utc = format(lubridate::with_tz(.data$obs_datetime, "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      api_latest_wl_date = as.Date(.data$obs_datetime),
      api_latest_wl_last_modified_utc = dplyr::if_else(
        is.na(.data$last_modified_utc),
        NA_character_,
        format(lubridate::with_tz(.data$last_modified_utc, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
      )
    ) |>
    dplyr::transmute(
      site_no = .data$site_no,
      api_latest_wl_ft_bgs = .data$obs_value,
      api_latest_wl_datetime_utc = .data$api_latest_wl_datetime_utc,
      api_latest_wl_date = .data$api_latest_wl_date,
      api_latest_wl_status = if ("approval_status" %in% names(raw2)) pt_chr(.data$approval_status) else NA_character_,
      api_latest_wl_procedure = .data$observing_procedure,
      api_latest_wl_qualifier = if ("qualifier" %in% names(raw2)) pt_chr(.data$qualifier) else NA_character_,
      api_latest_wl_units = .data$unit_of_measure,
      api_latest_wl_vertical_datum = .data$vertical_datum,
      api_latest_wl_measuring_agency = .data$measuring_agency,
      api_latest_wl_field_visit_id = .data$field_visit_id,
      api_latest_wl_last_modified_utc = .data$api_latest_wl_last_modified_utc
    )

  list(raw = raw, latest = latest)
}

pt_template_url <- function(template, id) {
  ifelse(!is.na(id) & id != "", paste0(template, id), NA_character_)
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

# ---- 4. Read groundwater candidate index -----------------------------------

if (!file.exists(station_index_csv)) {
  stop(
    "USGS groundwater station index CSV not found: ", station_index_csv,
    "\nRun source('02_preprocess/30_export_usgs_groundwater_live_inputs.R') from the BRIM project, ",
    "then commit data/input/usgs_groundwater_latest_index_ca.csv to the feed repo."
  )
}

station_index_raw <- readr::read_csv(
  station_index_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

station_index_raw <- pt_ensure_cols(
  station_index_raw,
  c(
    "site_no", "station_nm", "latitude", "longitude", "status",
    "latest_wl_ft_bgs", "latest_wl_datetime_utc", "latest_wl_date",
    "latest_age_days", "well_depth_ft", "hole_depth_ft",
    "screen_top_ft", "screen_bottom_ft",
    "aqfr_cd", "aqfr_type_cd", "nat_aqfr_cd",
    "usgs_monitoring_location_url", "usgs_gw_levels_url",
    "latest_wl_status", "latest_wl_procedure", "latest_wl_qualifier",
    "latest_wl_units", "latest_wl_source"
  )
)

station_index <- station_index_raw |>
  dplyr::transmute(
    site_no = pt_site_no(.data$site_no),
    station_nm = pt_chr(.data$station_nm),
    latitude = pt_num(.data$latitude),
    longitude = pt_num(.data$longitude),
    status = pt_chr(.data$status),

    index_latest_wl_ft_bgs = pt_num(.data$latest_wl_ft_bgs),
    index_latest_wl_datetime_utc = pt_chr(.data$latest_wl_datetime_utc),
    index_latest_wl_date = suppressWarnings(as.Date(.data$latest_wl_date)),
    index_latest_wl_status = pt_chr(.data$latest_wl_status),
    index_latest_wl_procedure = pt_chr(.data$latest_wl_procedure),
    index_latest_wl_qualifier = pt_chr(.data$latest_wl_qualifier),
    index_latest_wl_units = pt_chr(.data$latest_wl_units),
    index_latest_wl_source = pt_chr(.data$latest_wl_source),

    well_depth_ft = pt_num(.data$well_depth_ft),
    hole_depth_ft = pt_num(.data$hole_depth_ft),
    screen_top_ft = pt_num(.data$screen_top_ft),
    screen_bottom_ft = pt_num(.data$screen_bottom_ft),
    aqfr_cd = pt_chr(.data$aqfr_cd),
    aqfr_type_cd = pt_chr(.data$aqfr_type_cd),
    nat_aqfr_cd = pt_chr(.data$nat_aqfr_cd),

    usgs_monitoring_location_url = dplyr::coalesce(
      pt_chr(.data$usgs_monitoring_location_url),
      paste0("https://waterdata.usgs.gov/monitoring-location/USGS-", pt_site_no(.data$site_no), "/")
    ),
    usgs_gw_levels_url = dplyr::coalesce(
      pt_chr(.data$usgs_gw_levels_url),
      paste0("https://waterdata.usgs.gov/monitoring-location/USGS-", pt_site_no(.data$site_no), "/all-graphs")
    )
  ) |>
  dplyr::filter(!is.na(.data$site_no), !is.na(.data$latitude), !is.na(.data$longitude)) |>
  dplyr::arrange(.data$site_no) |>
  dplyr::distinct(.data$site_no, .keep_all = TRUE)

if (nrow(station_index) < min_features_to_publish) {
  stop(
    "Groundwater station index has only ", nrow(station_index), " site(s), below minimum ",
    min_features_to_publish, ". Refusing to publish."
  )
}

message("USGS groundwater station index rows: ", nrow(station_index))

# ---- 5. Read optional RF029 groundwater history summary ---------------------

history_summary <- tibble::tibble(site_no = character())
history_summary_rows <- 0L
history_summary_joined_fields <- character(0)

if (file.exists(history_summary_csv)) {
  history_summary_raw <- readr::read_csv(
    history_summary_csv,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  if (!"site_no" %in% names(history_summary_raw)) {
    warning("Groundwater history summary CSV exists but is missing site_no: ", history_summary_csv)
  } else {
    history_summary <- history_summary_raw |>
      dplyr::mutate(site_no = pt_site_no(.data$site_no)) |>
      dplyr::filter(!is.na(.data$site_no)) |>
      dplyr::distinct(.data$site_no, .keep_all = TRUE)

    history_summary_rows <- nrow(history_summary)
    history_summary_joined_fields <- setdiff(names(history_summary), "site_no")
    message("USGS groundwater RF029 history summary rows read: ", history_summary_rows)
  }
} else {
  message("No optional RF029 groundwater history summary CSV found: ", history_summary_csv)
}

# ---- 6. Fetch latest groundwater field measurements ------------------------

fetch_result <- pt_fetch_latest_groundwater(
  site_ids = station_index$site_no,
  start_date = query_start_date,
  end_date = query_end_date
)

api_latest <- fetch_result$latest
api_latest_count <- nrow(api_latest)

message("USGS groundwater API latest site count: ", api_latest_count)

if (api_latest_count < min_api_sites_to_publish) {
  stop(
    "USGS groundwater API returned latest values for only ", api_latest_count,
    " site(s), below minimum ", min_api_sites_to_publish,
    ". Refusing to publish a degraded feed."
  )
}

# ---- 7. Join and create feed table -----------------------------------------

latest_tbl <- station_index |>
  dplyr::left_join(api_latest, by = "site_no") |>
  dplyr::left_join(history_summary, by = "site_no") |>
  dplyr::mutate(
    has_api_latest_wl = !is.na(.data$api_latest_wl_ft_bgs),

    latest_wl_ft_bgs = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_ft_bgs,
      if (allow_index_fallback) .data$index_latest_wl_ft_bgs else NA_real_
    ),
    latest_wl_datetime_utc = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_datetime_utc,
      if (allow_index_fallback) .data$index_latest_wl_datetime_utc else NA_character_
    ),
    latest_wl_date = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_date,
      if (allow_index_fallback) .data$index_latest_wl_date else as.Date(NA)
    ),
    latest_wl_status = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_status,
      if (allow_index_fallback) .data$index_latest_wl_status else NA_character_
    ),
    latest_wl_procedure = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_procedure,
      if (allow_index_fallback) .data$index_latest_wl_procedure else NA_character_
    ),
    latest_wl_qualifier = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_qualifier,
      if (allow_index_fallback) .data$index_latest_wl_qualifier else NA_character_
    ),
    latest_wl_units = dplyr::if_else(
      .data$has_api_latest_wl,
      .data$api_latest_wl_units,
      if (allow_index_fallback) .data$index_latest_wl_units else NA_character_
    ),
    latest_wl_source = dplyr::if_else(
      .data$has_api_latest_wl,
      "USGS Water Data API field-measurements endpoint, parameter 72019",
      if (allow_index_fallback) paste0("Candidate-index fallback: ", dplyr::coalesce(.data$index_latest_wl_source, "prior BRIM export")) else NA_character_
    ),

    latest_age_days = as.numeric(run_date_utc - .data$latest_wl_date),
    latest_wl_depth_class = pt_depth_class(.data$latest_wl_ft_bgs),
    is_artesian_or_above_land_surface = !is.na(.data$latest_wl_ft_bgs) & .data$latest_wl_ft_bgs < 0,
    has_well_depth = !is.na(.data$well_depth_ft),
    has_hole_depth = !is.na(.data$hole_depth_ft),
    has_screen_interval = !is.na(.data$screen_top_ft) | !is.na(.data$screen_bottom_ft),
    has_aquifer_code = !is.na(.data$aqfr_cd) | !is.na(.data$aqfr_type_cd) | !is.na(.data$nat_aqfr_cd),

    latest_status = dplyr::case_when(
      is.na(.data$latest_wl_ft_bgs) ~ "no_recent_groundwater_level",
      !is.na(.data$latest_age_days) & .data$latest_age_days <= 90 ~ "latest_groundwater_level_90d",
      !is.na(.data$latest_age_days) & .data$latest_age_days <= 365 ~ "latest_groundwater_level_1y",
      !is.na(.data$latest_age_days) & .data$latest_age_days <= 365 * 2 ~ "latest_groundwater_level_2y",
      !is.na(.data$latest_age_days) & .data$latest_age_days <= field_measurements_lookback_days ~ "latest_groundwater_level_query_window",
      TRUE ~ "stale_or_index_fallback_groundwater_level"
    ),

    usgs_monitoring_location_url = paste0("https://waterdata.usgs.gov/monitoring-location/USGS-", .data$site_no, "/"),
    usgs_all_graphs_url = paste0("https://waterdata.usgs.gov/monitoring-location/USGS-", .data$site_no, "/all-graphs"),
    usgs_gw_levels_url = dplyr::coalesce(
      .data$usgs_gw_levels_url,
      .data$usgs_all_graphs_url
    ),

    feed_build_time_utc = feed_build_time_utc,
    feed_source = "USGS Water Data API field-measurements, parameter 72019, joined to BRIM groundwater candidate index",
    feed_scope = "California active/recent USGS groundwater candidates from BRIM static well index",
    field_measurements_query_start_date = query_start_date,
    field_measurements_query_end_date = query_end_date,
    field_measurements_lookback_days = field_measurements_lookback_days,
    data_quality_note = paste(
      "USGS groundwater field measurements are low-frequency site-visit measurements and may be provisional.",
      "Depth-to-water values use parameter 72019 when available and should be interpreted with well construction, datum, aquifer/screen interval, and measurement frequency."
    )
  ) |>
  dplyr::filter(!is.na(.data$latest_wl_ft_bgs)) |>
  dplyr::arrange(.data$site_no)

if (nrow(latest_tbl) < min_features_to_publish) {
  stop(
    "Groundwater output has only ", nrow(latest_tbl), " feature(s), below minimum ",
    min_features_to_publish, ". Refusing to publish."
  )
}

index_fallback_count <- sum(!latest_tbl$has_api_latest_wl, na.rm = TRUE)

message("USGS groundwater output feature count: ", nrow(latest_tbl))
message("USGS groundwater index-fallback feature count: ", index_fallback_count)

# ---- 8. Write GeoJSON and summary ------------------------------------------

features <- lapply(seq_len(nrow(latest_tbl)), function(i) pt_make_feature(latest_tbl[i, ]))

geojson <- list(
  type = "FeatureCollection",
  name = "USGS Groundwater Latest CA",
  metadata = list(
    feed_build_time_utc = feed_build_time_utc,
    station_index_csv = station_index_csv,
    history_summary_csv = ifelse(file.exists(history_summary_csv), history_summary_csv, NA_character_),
    history_summary_rows = history_summary_rows,
    scope = "CA",
    parameter_code = gw_parameter_code,
    retrieval_backend = "dataRetrieval::read_waterdata_field_measurements",
    field_measurements_lookback_days = field_measurements_lookback_days,
    allow_index_fallback = allow_index_fallback
  ),
  features = features
)

dir.create(dirname(out_geojson), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_summary), recursive = TRUE, showWarnings = FALSE)

jsonlite::write_json(
  geojson,
  out_geojson,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  digits = 8,
  pretty = FALSE
)

summary <- list(
  feed_build_time_utc = feed_build_time_utc,
  scope = "CA",
  retrieval_backend = "dataRetrieval::read_waterdata_field_measurements",
  parameter_code = gw_parameter_code,
  station_index_rows = nrow(station_index),
  history_summary_csv_found = file.exists(history_summary_csv),
  history_summary_rows = history_summary_rows,
  history_summary_joined_field_count = length(history_summary_joined_fields),
  history_sites_with_plot_json = if ("hist_plot_wy_mean_json" %in% names(latest_tbl)) sum(!is.na(latest_tbl$hist_plot_wy_mean_json) & latest_tbl$hist_plot_wy_mean_json != "", na.rm = TRUE) else 0L,
  history_sites_with_por_percentile = if ("hist_por_deeper_pctile" %in% names(latest_tbl)) sum(!is.na(latest_tbl$hist_por_deeper_pctile), na.rm = TRUE) else 0L,
  history_sites_with_seasonal_percentile = if ("hist_seasonal_deeper_pctile" %in% names(latest_tbl)) sum(!is.na(latest_tbl$hist_seasonal_deeper_pctile), na.rm = TRUE) else 0L,
  output_feature_count = nrow(latest_tbl),
  api_latest_site_count = api_latest_count,
  index_fallback_count = index_fallback_count,
  raw_field_measurements_rows = nrow(fetch_result$raw),
  field_measurements_query_start_date = query_start_date,
  field_measurements_query_end_date = query_end_date,
  field_measurements_lookback_days = field_measurements_lookback_days,
  latest_90d_count = sum(!is.na(latest_tbl$latest_age_days) & latest_tbl$latest_age_days <= 90, na.rm = TRUE),
  latest_1y_count = sum(!is.na(latest_tbl$latest_age_days) & latest_tbl$latest_age_days <= 365, na.rm = TRUE),
  latest_2y_count = sum(!is.na(latest_tbl$latest_age_days) & latest_tbl$latest_age_days <= 365 * 2, na.rm = TRUE),
  well_depth_count = sum(!is.na(latest_tbl$well_depth_ft), na.rm = TRUE),
  hole_depth_count = sum(!is.na(latest_tbl$hole_depth_ft), na.rm = TRUE),
  screen_interval_count = sum(latest_tbl$has_screen_interval, na.rm = TRUE),
  aquifer_code_count = sum(latest_tbl$has_aquifer_code, na.rm = TRUE),
  min_latest_wl_ft_bgs = suppressWarnings(min(latest_tbl$latest_wl_ft_bgs, na.rm = TRUE)),
  max_latest_wl_ft_bgs = suppressWarnings(max(latest_tbl$latest_wl_ft_bgs, na.rm = TRUE)),
  mean_latest_wl_ft_bgs = suppressWarnings(mean(latest_tbl$latest_wl_ft_bgs, na.rm = TRUE)),
  min_latest_age_days = suppressWarnings(min(latest_tbl$latest_age_days, na.rm = TRUE)),
  max_latest_age_days = suppressWarnings(max(latest_tbl$latest_age_days, na.rm = TRUE)),
  allow_index_fallback = allow_index_fallback,
  notes = c(
    "Latest groundwater levels are from USGS Water Data API field-measurements parameter 72019 where available.",
    "The BRIM input index provides stable site locations and well construction/aquifer fields.",
    "Index fallback is used only for sites without an API result in the query window when enabled; fallback rows are flagged with has_api_latest_wl = false.",
    "Screen/perforation interval fields are preserved if available in the input, but the current public USGS API/index fields may not provide populated screen interval fields.",
    "USGS groundwater field measurements are low-frequency and may be provisional; interpret values with well construction, datum, aquifer/screen interval, and measurement history.",
    "If RF029 history summary CSV is present, compact water-year history and percentile fields are joined into the hosted GeoJSON for popup/mini-plot use."
  )
)

if (!is.finite(summary$min_latest_wl_ft_bgs)) summary$min_latest_wl_ft_bgs <- NA_real_
if (!is.finite(summary$max_latest_wl_ft_bgs)) summary$max_latest_wl_ft_bgs <- NA_real_
if (!is.finite(summary$mean_latest_wl_ft_bgs)) summary$mean_latest_wl_ft_bgs <- NA_real_
if (!is.finite(summary$min_latest_age_days)) summary$min_latest_age_days <- NA_real_
if (!is.finite(summary$max_latest_age_days)) summary$max_latest_age_days <- NA_real_

jsonlite::write_json(
  summary,
  out_summary,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  digits = 8,
  pretty = TRUE
)

message("Saved GeoJSON: ", out_geojson)
message("Saved summary: ", out_summary)
message("USGS groundwater live feed complete.")
print(summary)
