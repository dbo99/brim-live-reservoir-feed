# ==== build_usgs_streamflow_latest_ca.R ======================================
#
# PURPOSE:
#   Fetch latest/recent USGS California streamflow values and write a small
#   GeoJSON feed for BRIM Ops Live.
#
# OUTPUTS:
#   docs/data/usgs_streamflow_latest_ca.geojson
#   docs/data/usgs_streamflow_latest_ca_summary.json
#
# DESIGN:
#   - The static station index is committed to the live-data feed repo.
#   - Latest values come from USGS Water Data API latest-continuous data,
#     parameter 00060 discharge and 00065 gage height where available.
#   - Daily-value data from the modern Water Data API are used only for
#     optional compact recent-history context, not as the primary/latest
#     real-time observation.
#   - The output is static GeoJSON, suitable for GitHub Pages.
#
# HOW TO RUN LOCALLY FROM THE FEED REPOSITORY:
#   Rscript scripts/build_usgs_streamflow_latest_ca.R
#
# REQUIRED INPUT:
#   data/input/usgs_streamgages_index_ca.csv
#
# OPTIONAL INPUT:
#   data/input/usgs_cnrfc_nwsli_crosswalk.csv
#
# NOTES:
#   USGS provisional real-time data are subject to revision.  BRIM should treat
#   this as screening/situational-awareness information, not an operational
#   forecast product.
# ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c(
  "dataRetrieval", "dplyr", "readr", "lubridate", "jsonlite", "tibble", "purrr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running locally, or let the GitHub Action install them."
  )
}

required_dataretrieval_funs <- c(
  "read_waterdata_latest_continuous",
  "read_waterdata_daily"
)

missing_dataretrieval_funs <- required_dataretrieval_funs[
  !vapply(required_dataretrieval_funs, exists, logical(1), where = asNamespace("dataRetrieval"), inherits = FALSE)
]

if (length(missing_dataretrieval_funs) > 0) {
  stop(
    "Installed dataRetrieval package is too old for RF024c. Missing function(s): ",
    paste(missing_dataretrieval_funs, collapse = ", "),
    "\nUpdate dataRetrieval from CRAN, then rerun."
  )
}

## RF024d:
##   Some versions of dataRetrieval/cli can throw progress-bar formatting
##   errors inside the modern Water Data API helpers, especially during larger
##   daily-data chunk requests.  The feed does its own simple console progress,
##   so suppress package progress UI where possible and keep direct API
##   fallbacks below for daily chunks if dataRetrieval still trips over cli.
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
  library(purrr)
})

# ---- 2. Paths, constants, and switches -------------------------------------

station_index_csv <- Sys.getenv(
  "USGS_STREAMFLOW_STATION_INDEX_CSV",
  unset = "data/input/usgs_streamgages_index_ca.csv"
)

nwsli_crosswalk_csv <- Sys.getenv(
  "USGS_CNRFC_NWSLI_CROSSWALK_CSV",
  unset = "data/input/usgs_cnrfc_nwsli_crosswalk.csv"
)

out_geojson <- Sys.getenv(
  "USGS_STREAMFLOW_GEOJSON",
  unset = "docs/data/usgs_streamflow_latest_ca.geojson"
)

out_summary <- Sys.getenv(
  "USGS_STREAMFLOW_SUMMARY_JSON",
  unset = "docs/data/usgs_streamflow_latest_ca_summary.json"
)

iv_lookback_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_STREAMFLOW_IV_LOOKBACK_DAYS",
  unset = "3"
)))
if (is.na(iv_lookback_days) || iv_lookback_days < 1) iv_lookback_days <- 3L

history_mode <- Sys.getenv(
  "USGS_STREAMFLOW_HISTORY_MODE",
  unset = "dv_3day"
)
history_mode <- tolower(trimws(history_mode))
if (!history_mode %in% c("none", "dv_3day")) history_mode <- "dv_3day"

history_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_STREAMFLOW_HISTORY_DAYS",
  unset = "3"
)))
if (is.na(history_days) || history_days < 1) history_days <- 3L

chunk_size <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_STREAMFLOW_CHUNK_SIZE",
  unset = "80"
)))
if (is.na(chunk_size) || chunk_size < 1) chunk_size <- 80L

request_pause_sec <- suppressWarnings(as.numeric(Sys.getenv(
  "USGS_STREAMFLOW_REQUEST_PAUSE_SEC",
  unset = "0.15"
)))
if (is.na(request_pause_sec) || request_pause_sec < 0) request_pause_sec <- 0.15

min_sites_to_publish <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_STREAMFLOW_MIN_SITES_TO_PUBLISH",
  unset = "100"
)))
if (is.na(min_sites_to_publish) || min_sites_to_publish < 1) min_sites_to_publish <- 100L

feed_build_time <- Sys.time()
feed_build_time_utc <- format(lubridate::with_tz(feed_build_time, "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# ---- 3. Helpers -------------------------------------------------------------

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
    if (!nm %in% names(df)) df[[nm]] <- NA
  }
  df
}

pt_chunks <- function(x, n) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & x != ""]
  split(x, ceiling(seq_along(x) / n))
}

pt_find_param_value_col <- function(df, parameter_cd) {
  nm <- names(df)
  hits <- nm[grepl(parameter_cd, nm, fixed = TRUE)]
  hits <- hits[!grepl("_cd$|_qa$|_qual|tz_cd|agency_cd|site_no|dateTime|Date", hits, ignore.case = TRUE)]

  if (length(hits) == 0) return(NA_character_)

  numeric_hits <- hits[vapply(hits, function(h) is.numeric(df[[h]]) || is.integer(df[[h]]), logical(1))]
  if (length(numeric_hits) > 0) return(numeric_hits[[1]])

  hits[[1]]
}

pt_find_param_cd_col <- function(df, value_col, parameter_cd) {
  if (!is.na(value_col) && paste0(value_col, "_cd") %in% names(df)) {
    return(paste0(value_col, "_cd"))
  }

  nm <- names(df)
  hits <- nm[grepl(parameter_cd, nm, fixed = TRUE) & grepl("_cd$", nm)]
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

pt_date_col <- function(df) {
  if ("Date" %in% names(df)) return("Date")
  if ("dateTime" %in% names(df)) return("dateTime")
  NA_character_
}

pt_empty_iv_latest <- function() {
  tibble::tibble(
    site_no = character(),
    q_cfs = numeric(),
    q_datetime_utc = character(),
    q_cd = character(),
    q_obs_age_hours = numeric(),
    stage_ft = numeric(),
    stage_datetime_utc = character(),
    stage_cd = character(),
    stage_obs_age_hours = numeric()
  )
}

pt_empty_dv_summary <- function() {
  tibble::tibble(
    site_no = character(),
    history_source = character(),
    history_days_requested = integer(),
    q_hist_n = integer(),
    q_hist_start_date = character(),
    q_hist_end_date = character(),
    q_3day_min_cfs = numeric(),
    q_3day_max_cfs = numeric(),
    q_3day_mean_cfs = numeric(),
    q_3day_first_cfs = numeric(),
    q_3day_latest_cfs = numeric(),
    q_3day_change_cfs = numeric(),
    q_3day_change_pct = numeric()
  )
}


pt_usgs_monitoring_location_id <- function(site_ids) {
  site_ids <- pt_site_no(site_ids)
  out <- ifelse(!is.na(site_ids) & site_ids != "", paste0("USGS-", site_ids), NA_character_)
  out[!is.na(out)]
}

pt_waterdata_time_interval <- function(start_date, end_date) {
  paste0(as.character(start_date), "/", as.character(end_date))
}

pt_waterdata_latest_duration <- function(days) {
  days <- suppressWarnings(as.integer(days))
  if (is.na(days) || days < 1) days <- 3L
  paste0("P", days, "D")
}

pt_has_cols <- function(df, cols) {
  all(cols %in% names(df))
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

  x <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = TRUE),
    error = function(e) e
  )

  if (inherits(x, "error")) {
    warning("Direct Water Data API fallback failed for ", label, ": ", conditionMessage(x))
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

  if (is.null(props)) {
    return(tibble::tibble())
  }

  tibble::as_tibble(props)
}

pt_fetch_daily_chunk_direct <- function(site_ids, start_date, end_date, label) {
  url <- pt_ogc_query_url(
    collection = "daily",
    params = list(
      f = "json",
      lang = "en-US",
      skipGeometry = "TRUE",
      properties = paste(
        c(
          "monitoring_location_id",
          "parameter_code",
          "statistic_id",
          "time",
          "value",
          "unit_of_measure",
          "qualifier",
          "approval_status",
          "last_modified"
        ),
        collapse = ","
      ),
      monitoring_location_id = paste(pt_usgs_monitoring_location_id(site_ids), collapse = ","),
      parameter_code = "00060",
      statistic_id = "00003",
      time = pt_waterdata_time_interval(start_date, end_date),
      limit = "50000"
    )
  )

  pt_fetch_ogc_properties(url, label = paste0("daily chunk ", label))
}

pt_compact_code <- function(qualifier, approval_status) {
  qualifier <- pt_chr(qualifier)
  approval_status <- pt_chr(approval_status)

  out <- dplyr::case_when(
    !is.na(qualifier) & !is.na(approval_status) ~ paste0(qualifier, "; ", approval_status),
    !is.na(qualifier) ~ qualifier,
    !is.na(approval_status) ~ approval_status,
    TRUE ~ NA_character_
  )

  out
}

pt_fetch_latest_continuous_chunk <- function(site_ids, time_filter, label) {
  ## RF024c: use the modern USGS Water Data API via dataRetrieval rather than
  ## legacy readNWISuv().  The latest-continuous endpoint returns the most
  ## recent continuous/IV observation within the requested time filter for each
  ## monitoring location/parameter where available.
  tryCatch({
    dataRetrieval::read_waterdata_latest_continuous(
      monitoring_location_id = pt_usgs_monitoring_location_id(site_ids),
      parameter_code = c("00060", "00065"),
      time = time_filter,
      properties = c(
        "monitoring_location_id",
        "parameter_code",
        "time",
        "value",
        "unit_of_measure",
        "qualifier",
        "approval_status",
        "last_modified"
      ),
      skipGeometry = TRUE
    )
  }, error = function(e) {
    warning("USGS latest-continuous chunk failed for ", label, ": ", conditionMessage(e))
    tibble::tibble()
  })
}

pt_fetch_daily_chunk <- function(site_ids, start_date, end_date, label) {
  ## RF024c: use the modern daily Water Data API rather than legacy
  ## readNWISdv(). Daily data are used only for compact history/context.
  ## RF024d: if dataRetrieval fails because of cli/progress formatting, retry
  ## the same OGC API request directly and return its properties table.
  tryCatch({
    dataRetrieval::read_waterdata_daily(
      monitoring_location_id = pt_usgs_monitoring_location_id(site_ids),
      parameter_code = "00060",
      statistic_id = "00003",
      time = pt_waterdata_time_interval(start_date, end_date),
      properties = c(
        "monitoring_location_id",
        "parameter_code",
        "statistic_id",
        "time",
        "value",
        "unit_of_measure",
        "qualifier",
        "approval_status",
        "last_modified"
      ),
      skipGeometry = TRUE
    )
  }, error = function(e) {
    warning(
      "USGS daily Water Data API chunk failed through dataRetrieval for ",
      label,
      ": ",
      conditionMessage(e),
      ". Trying direct OGC API fallback."
    )
    pt_fetch_daily_chunk_direct(
      site_ids = site_ids,
      start_date = start_date,
      end_date = end_date,
      label = label
    )
  })
}

pt_fetch_iv_latest <- function(site_ids, start_date, end_date) {
  chunks <- pt_chunks(site_ids, chunk_size)
  latest_time_filter <- pt_waterdata_latest_duration(iv_lookback_days)

  message("Fetching USGS latest-continuous values in ", length(chunks), " chunk(s).")
  message("USGS latest-continuous time filter: ", latest_time_filter)

  out <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    message("  latest-continuous chunk ", i, " of ", length(chunks), " | sites: ", length(chunks[[i]]))
    out[[i]] <- pt_fetch_latest_continuous_chunk(
      site_ids = chunks[[i]],
      time_filter = latest_time_filter,
      label = paste0(i, "/", length(chunks))
    )
    if (i < length(chunks) && request_pause_sec > 0) Sys.sleep(request_pause_sec)
  }

  raw <- dplyr::bind_rows(out)
  message("USGS latest-continuous raw rows fetched: ", nrow(raw))

  if (nrow(raw) == 0) {
    return(pt_empty_iv_latest())
  }

  needed <- c("monitoring_location_id", "parameter_code", "time", "value")
  if (!pt_has_cols(raw, needed)) {
    warning(
      "USGS latest-continuous output did not include expected columns: ",
      paste(setdiff(needed, names(raw)), collapse = ", "),
      ". Skipping latest IV values."
    )
    return(pt_empty_iv_latest())
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
      )
    ) |>
    dplyr::filter(!is.na(.data$site_no), !is.na(.data$obs_datetime), !is.na(.data$obs_value))

  q_latest <- raw2 |>
    dplyr::filter(.data$parameter_code == "00060") |>
    dplyr::arrange(.data$site_no, .data$obs_datetime) |>
    dplyr::group_by(.data$site_no) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      q_datetime_utc = format(lubridate::with_tz(.data$obs_datetime, "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      q_obs_age_hours = as.numeric(difftime(feed_build_time, .data$obs_datetime, units = "hours"))
    ) |>
    dplyr::transmute(
      site_no = .data$site_no,
      q_cfs = .data$obs_value,
      q_datetime_utc = .data$q_datetime_utc,
      q_cd = .data$obs_code,
      q_obs_age_hours = .data$q_obs_age_hours
    )

  stage_latest <- raw2 |>
    dplyr::filter(.data$parameter_code == "00065") |>
    dplyr::arrange(.data$site_no, .data$obs_datetime) |>
    dplyr::group_by(.data$site_no) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      stage_datetime_utc = format(lubridate::with_tz(.data$obs_datetime, "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      stage_obs_age_hours = as.numeric(difftime(feed_build_time, .data$obs_datetime, units = "hours"))
    ) |>
    dplyr::transmute(
      site_no = .data$site_no,
      stage_ft = .data$obs_value,
      stage_datetime_utc = .data$stage_datetime_utc,
      stage_cd = .data$obs_code,
      stage_obs_age_hours = .data$stage_obs_age_hours
    )

  out <- dplyr::full_join(q_latest, stage_latest, by = "site_no")

  if (nrow(out) == 0) return(pt_empty_iv_latest())

  out
}

pt_fetch_dv_summary <- function(site_ids, start_date, end_date) {
  if (history_mode == "none") {
    message("USGS streamflow history mode is none; skipping daily context.")
    return(pt_empty_dv_summary())
  }

  chunks <- pt_chunks(site_ids, chunk_size)
  message("Fetching USGS daily history in ", length(chunks), " chunk(s).")
  message("USGS daily history time interval: ", pt_waterdata_time_interval(start_date, end_date))

  out <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    message("  daily chunk ", i, " of ", length(chunks), " | sites: ", length(chunks[[i]]))
    out[[i]] <- pt_fetch_daily_chunk(
      site_ids = chunks[[i]],
      start_date = start_date,
      end_date = end_date,
      label = paste0(i, "/", length(chunks))
    )
    if (i < length(chunks) && request_pause_sec > 0) Sys.sleep(request_pause_sec)
  }

  raw <- dplyr::bind_rows(out)
  message("USGS daily raw rows fetched: ", nrow(raw))

  if (nrow(raw) == 0) {
    return(pt_empty_dv_summary())
  }

  needed <- c("monitoring_location_id", "parameter_code", "time", "value")
  if (!pt_has_cols(raw, needed)) {
    warning(
      "USGS daily output did not include expected columns: ",
      paste(setdiff(needed, names(raw)), collapse = ", "),
      ". Skipping daily history summary."
    )
    return(pt_empty_dv_summary())
  }

  raw |>
    dplyr::mutate(
      site_no = pt_site_no(.data$monitoring_location_id),
      parameter_code = as.character(.data$parameter_code),
      statistic_id = if ("statistic_id" %in% names(raw)) as.character(.data$statistic_id) else "00003",
      q_date = suppressWarnings(as.Date(.data$time)),
      q_daily_mean_cfs = pt_num(.data$value)
    ) |>
    dplyr::filter(
      !is.na(.data$site_no),
      .data$parameter_code == "00060",
      .data$statistic_id == "00003",
      !is.na(.data$q_date),
      !is.na(.data$q_daily_mean_cfs)
    ) |>
    dplyr::arrange(.data$site_no, .data$q_date) |>
    dplyr::group_by(.data$site_no) |>
    dplyr::summarise(
      history_source = "USGS Water Data API daily mean 00060/00003",
      history_days_requested = history_days,
      q_hist_n = dplyr::n(),
      q_hist_start_date = as.character(min(.data$q_date, na.rm = TRUE)),
      q_hist_end_date = as.character(max(.data$q_date, na.rm = TRUE)),
      q_3day_min_cfs = min(.data$q_daily_mean_cfs, na.rm = TRUE),
      q_3day_max_cfs = max(.data$q_daily_mean_cfs, na.rm = TRUE),
      q_3day_mean_cfs = mean(.data$q_daily_mean_cfs, na.rm = TRUE),
      q_3day_first_cfs = dplyr::first(.data$q_daily_mean_cfs),
      q_3day_latest_cfs = dplyr::last(.data$q_daily_mean_cfs),
      q_3day_change_cfs = .data$q_3day_latest_cfs - .data$q_3day_first_cfs,
      q_3day_change_pct = dplyr::if_else(
        is.finite(.data$q_3day_first_cfs) & abs(.data$q_3day_first_cfs) > 0,
        100 * (.data$q_3day_latest_cfs - .data$q_3day_first_cfs) / .data$q_3day_first_cfs,
        NA_real_
      ),
      .groups = "drop"
    )
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

# ---- 4. Read static station index and optional crosswalk --------------------

if (!file.exists(station_index_csv)) {
  stop(
    "USGS streamflow station index CSV not found: ", station_index_csv,
    "\nRun source('02_preprocess/29_export_usgs_streamflow_live_inputs.R') from the BRIM project, ",
    "then commit data/input/usgs_streamgages_index_ca.csv to the feed repo."
  )
}

station_index <- readr::read_csv(
  station_index_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

station_index <- pt_ensure_cols(
  station_index,
  c(
    "site_no", "station_nm", "latitude", "longitude", "elev_ft", "site_type",
    "start_date", "end_date", "count_nu", "status", "param_list",
    "measurements", "source"
  )
)

station_index <- station_index |>
  dplyr::transmute(
    site_no = pt_site_no(.data$site_no),
    station_nm = pt_chr(.data$station_nm),
    latitude = pt_num(.data$latitude),
    longitude = pt_num(.data$longitude),
    elev_ft = pt_num(.data$elev_ft),
    site_type = pt_chr(.data$site_type),
    start_date = pt_chr(.data$start_date),
    end_date = pt_chr(.data$end_date),
    count_nu = suppressWarnings(as.integer(.data$count_nu)),
    status = pt_chr(.data$status),
    param_list = pt_chr(.data$param_list),
    measurements = pt_chr(.data$measurements),
    source = dplyr::coalesce(pt_chr(.data$source), "USGS streamgage")
  ) |>
  dplyr::filter(!is.na(.data$site_no), !is.na(.data$latitude), !is.na(.data$longitude)) |>
  dplyr::arrange(.data$site_no) |>
  dplyr::distinct(.data$site_no, .keep_all = TRUE)

if (nrow(station_index) < min_sites_to_publish) {
  stop(
    "Station index has only ", nrow(station_index), " site(s), below minimum ",
    min_sites_to_publish, ". Refusing to publish."
  )
}

message("USGS streamflow station index rows: ", nrow(station_index))

xwalk <- tibble::tibble(
  site_no = character(),
  nwsli = character(),
  cdec_id = character(),
  cdec_station_name = character(),
  usgs_station_name_xwalk = character(),
  cdec_group = character(),
  cdec_basin = character(),
  county = character(),
  nws_flood_stage_ft = numeric(),
  cnrfc_channel = character(),
  cnrfc_location = character(),
  cnrfc_nickname = character(),
  cnrfc_gage_class1 = character(),
  cnrfc_gage_class2 = character(),
  crosswalk_source = character(),
  crosswalk_note = character()
)

if (file.exists(nwsli_crosswalk_csv)) {
  xwalk_raw <- readr::read_csv(
    nwsli_crosswalk_csv,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  xwalk_raw <- pt_ensure_cols(xwalk_raw, names(xwalk))

  xwalk <- xwalk_raw |>
    dplyr::transmute(
      site_no = pt_site_no(.data$site_no),
      nwsli = toupper(pt_chr(.data$nwsli)),
      cdec_id = toupper(pt_chr(.data$cdec_id)),
      cdec_station_name = pt_chr(.data$cdec_station_name),
      usgs_station_name_xwalk = pt_chr(.data$usgs_station_name_xwalk),
      cdec_group = pt_chr(.data$cdec_group),
      cdec_basin = pt_chr(.data$cdec_basin),
      county = pt_chr(.data$county),
      nws_flood_stage_ft = pt_num(.data$nws_flood_stage_ft),
      cnrfc_channel = pt_chr(.data$cnrfc_channel),
      cnrfc_location = pt_chr(.data$cnrfc_location),
      cnrfc_nickname = pt_chr(.data$cnrfc_nickname),
      cnrfc_gage_class1 = pt_chr(.data$cnrfc_gage_class1),
      cnrfc_gage_class2 = pt_chr(.data$cnrfc_gage_class2),
      crosswalk_source = pt_chr(.data$crosswalk_source),
      crosswalk_note = pt_chr(.data$crosswalk_note)
    ) |>
    dplyr::filter(!is.na(.data$site_no), !is.na(.data$nwsli), .data$nwsli != "") |>
    dplyr::distinct(.data$site_no, .keep_all = TRUE)
}

message("USGS-CNRFC/NWSLI crosswalk rows read: ", nrow(xwalk))

# ---- 5. Fetch latest continuous values and optional daily context -----------

run_date_utc <- as.Date(lubridate::with_tz(feed_build_time, "UTC"))
iv_start <- as.character(run_date_utc - iv_lookback_days)
iv_end <- as.character(run_date_utc + 1)

dv_start <- as.character(run_date_utc - history_days)
dv_end <- as.character(run_date_utc)

latest_iv <- pt_fetch_iv_latest(
  site_ids = station_index$site_no,
  start_date = iv_start,
  end_date = iv_end
)

message("USGS IV latest sites with discharge: ", sum(!is.na(latest_iv$q_cfs)))
message("USGS IV latest sites with gage height: ", sum(!is.na(latest_iv$stage_ft)))

history_dv <- pt_fetch_dv_summary(
  site_ids = station_index$site_no,
  start_date = dv_start,
  end_date = dv_end
)

message("USGS DV history sites summarized: ", nrow(history_dv))

# ---- 6. Join and create feed table -----------------------------------------

latest_tbl <- station_index |>
  dplyr::left_join(latest_iv, by = "site_no") |>
  dplyr::left_join(history_dv, by = "site_no") |>
  dplyr::left_join(xwalk, by = "site_no") |>
  dplyr::mutate(
    has_latest_iv_q = !is.na(.data$q_cfs),
    has_latest_iv_stage = !is.na(.data$stage_ft),
    has_nwsli = !is.na(.data$nwsli) & .data$nwsli != "",
    q_stale_6h = !is.na(.data$q_obs_age_hours) & .data$q_obs_age_hours > 6,
    q_stale_24h = !is.na(.data$q_obs_age_hours) & .data$q_obs_age_hours > 24,
    q_stale_72h = !is.na(.data$q_obs_age_hours) & .data$q_obs_age_hours > 72,
    latest_status = dplyr::case_when(
      .data$has_latest_iv_q & !.data$q_stale_24h ~ "recent_iv_discharge",
      .data$has_latest_iv_q & .data$q_stale_24h ~ "stale_iv_discharge",
      .data$has_latest_iv_stage ~ "stage_only_recent_iv",
      TRUE ~ "no_recent_iv_discharge"
    ),
    usgs_monitoring_location_url = paste0("https://waterdata.usgs.gov/monitoring-location/USGS-", .data$site_no, "/"),
    usgs_7day_flow_plot_url = paste0(
      "https://waterdata.usgs.gov/monitoring-location/USGS-", .data$site_no,
      "/#dataTypeId=continuous-00060-0&period=P7D&showFieldMeasurements=true"
    ),
    usgs_hydrograph_url = .data$usgs_7day_flow_plot_url,
    usgs_rating_stac_url = paste0(
      "https://api.waterdata.usgs.gov/stac-files/ratings/USGS.", .data$site_no, ".exsa.rdb"
    ),
    usgs_rating_depot_url = paste0(
      "https://waterdata.usgs.gov/nwisweb/get_ratings?file_type=exsa&site_no=", .data$site_no
    ),
    cnrfc_obs_url = dplyr::if_else(
      .data$has_nwsli,
      paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", .data$nwsli),
      NA_character_
    ),
    cnrfc_forecast_url = dplyr::if_else(
      .data$has_nwsli,
      paste0("https://www.cnrfc.noaa.gov/graphicalRVF.php?id=", .data$nwsli),
      NA_character_
    ),
    hads_metadata_url = dplyr::if_else(
      .data$has_nwsli,
      paste0("https://hads.ncep.noaa.gov/cgi-bin/hads/interactiveDisplays/displayMetaData.pl?table=dcp&nwsli=", .data$nwsli),
      NA_character_
    ),
    aprfc_gage_analysis_url = dplyr::if_else(
      .data$has_nwsli,
      paste0("https://www.weather.gov/source/aprfc/gageAnalysis.html?site=", tolower(.data$nwsli)),
      NA_character_
    ),
    feed_build_time_utc = feed_build_time_utc,
    feed_source = "USGS Water Data API latest-continuous values; optional USGS Water Data API daily-mean recent context",
    feed_scope = "California USGS streamgages from BRIM static station index",
    iv_query_start_date = iv_start,
    iv_query_end_date = iv_end,
    history_mode = history_mode,
    history_query_start_date = ifelse(history_mode == "none", NA_character_, dv_start),
    history_query_end_date = ifelse(history_mode == "none", NA_character_, dv_end),
    data_quality_note = "USGS real-time/instantaneous data are provisional and subject to revision. Daily-value context, when present, is a compact history summary and not the primary latest observation."
  ) |>
  dplyr::arrange(.data$site_no)

# ---- 7. Write GeoJSON and summary ------------------------------------------

features <- purrr::map(seq_len(nrow(latest_tbl)), function(i) pt_make_feature(latest_tbl[i, ]))

geojson <- list(
  type = "FeatureCollection",
  name = "USGS Streamflow Latest CA",
  metadata = list(
    feed_build_time_utc = feed_build_time_utc,
    station_index_csv = station_index_csv,
    nwsli_crosswalk_csv = ifelse(file.exists(nwsli_crosswalk_csv), nwsli_crosswalk_csv, NA_character_),
    scope = "CA",
    latest_continuous_parameters = c("00060", "00065"),
    latest_continuous_lookback_days = iv_lookback_days,
    retrieval_backend = "dataRetrieval::read_waterdata_latest_continuous + read_waterdata_daily",
    history_mode = history_mode,
    history_days = history_days
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
  retrieval_backend = "dataRetrieval::read_waterdata_latest_continuous + read_waterdata_daily",
  station_index_rows = nrow(station_index),
  output_feature_count = nrow(latest_tbl),
  iv_query_start_date = iv_start,
  iv_query_end_date = iv_end,
  iv_lookback_days = iv_lookback_days,
  latest_iv_discharge_count = sum(latest_tbl$has_latest_iv_q, na.rm = TRUE),
  latest_iv_stage_count = sum(latest_tbl$has_latest_iv_stage, na.rm = TRUE),
  stale_discharge_6h_count = sum(latest_tbl$q_stale_6h, na.rm = TRUE),
  stale_discharge_24h_count = sum(latest_tbl$q_stale_24h, na.rm = TRUE),
  stale_discharge_72h_count = sum(latest_tbl$q_stale_72h, na.rm = TRUE),
  no_recent_iv_discharge_count = sum(latest_tbl$latest_status == "no_recent_iv_discharge", na.rm = TRUE),
  nwsli_crosswalk_count = sum(latest_tbl$has_nwsli, na.rm = TRUE),
  history_mode = history_mode,
  history_query_start_date = ifelse(history_mode == "none", NA_character_, dv_start),
  history_query_end_date = ifelse(history_mode == "none", NA_character_, dv_end),
  dv_history_count = sum(!is.na(latest_tbl$q_hist_n), na.rm = TRUE),
  max_q_cfs = suppressWarnings(max(latest_tbl$q_cfs, na.rm = TRUE)),
  min_q_cfs = suppressWarnings(min(latest_tbl$q_cfs, na.rm = TRUE)),
  notes = c(
    "Latest discharge and stage values are from USGS Water Data API latest-continuous values where available.",
    "USGS Water Data API daily values are used only for compact recent-history context; daily chunks can fall back to direct OGC API calls if dataRetrieval progress UI fails.",
    "CNRFC/HADS/APRFC links are included only where the optional NWSLI crosswalk provides an ID.",
    "USGS rating-table links are generated from official USGS rating-file URL templates and may be unavailable for sites without traditional stage-discharge ratings."
  )
)

if (!is.finite(summary$max_q_cfs)) summary$max_q_cfs <- NA_real_
if (!is.finite(summary$min_q_cfs)) summary$min_q_cfs <- NA_real_

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
message("USGS streamflow live feed complete.")
print(summary)
