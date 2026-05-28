# ==== build_cocorahs_daily_precip_latest.R ==================================
#
# PURPOSE:
#   Fetch recent CoCoRaHS daily precipitation observations and write a small
#   static GeoJSON feed for BRIM Ops Live to load from GitHub Pages.
#
# OUTPUTS:
#   docs/data/cocorahs_daily_precip_latest.geojson
#   docs/data/cocorahs_daily_precip_latest_summary.json
#
# DESIGN:
#   - CoCoRaHS API calls happen in GitHub Actions, not in each user's browser.
#   - The browser loads a static GeoJSON file, avoiding local/file:// CORS
#     restrictions that blocked direct API fetches from standalone BRIM HTML.
#   - RF011 starts with California only.  Additional western states can be added
#     later by changing COCORAHS_STATES.
#
# HOW TO RUN LOCALLY FROM THIS REPOSITORY:
#   Rscript scripts/build_cocorahs_daily_precip_latest.R
# ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c("curl", "jsonlite", "lubridate", "tibble", "dplyr", "purrr", "readr")

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running locally, or let the GitHub Action install them."
  )
}

suppressPackageStartupMessages({
  library(curl)
  library(jsonlite)
  library(lubridate)
  library(tibble)
  library(dplyr)
  library(purrr)
  library(readr)
})

# ---- 2. Paths and constants -------------------------------------------------

cocorahs_api_url <- Sys.getenv(
  "COCORAHS_DAILY_PRECIP_API_URL",
  unset = "https://api2.cocorahs.org/api/DailyPrecipObs"
)

out_geojson <- Sys.getenv(
  "COCORAHS_DAILY_PRECIP_GEOJSON",
  unset = "docs/data/cocorahs_daily_precip_latest.geojson"
)

out_summary <- Sys.getenv(
  "COCORAHS_DAILY_PRECIP_SUMMARY_JSON",
  unset = "docs/data/cocorahs_daily_precip_latest_summary.json"
)

cocorahs_states <- strsplit(Sys.getenv(
  "COCORAHS_STATES",
  unset = "CA"
), ",", fixed = TRUE)[[1]]

cocorahs_states <- toupper(trimws(cocorahs_states))
cocorahs_states <- cocorahs_states[nzchar(cocorahs_states)]

if (length(cocorahs_states) == 0) {
  cocorahs_states <- "CA"
}

cocorahs_units <- Sys.getenv("COCORAHS_UNITS", unset = "english")

cocorahs_limit <- suppressWarnings(as.integer(Sys.getenv(
  "COCORAHS_PAGE_LIMIT",
  unset = "5000"
)))

if (is.na(cocorahs_limit) || cocorahs_limit < 1) {
  cocorahs_limit <- 5000L
}

cocorahs_max_pages_per_state <- suppressWarnings(as.integer(Sys.getenv(
  "COCORAHS_MAX_PAGES_PER_STATE",
  unset = "10"
)))

if (is.na(cocorahs_max_pages_per_state) || cocorahs_max_pages_per_state < 1) {
  cocorahs_max_pages_per_state <- 10L
}

cocorahs_request_pause_sec <- suppressWarnings(as.numeric(Sys.getenv(
  "COCORAHS_REQUEST_PAUSE_SEC",
  unset = "0.25"
)))

if (is.na(cocorahs_request_pause_sec) || cocorahs_request_pause_sec < 0) {
  cocorahs_request_pause_sec <- 0.25
}

lookback_days <- suppressWarnings(as.integer(Sys.getenv(
  "COCORAHS_LOOKBACK_DAYS",
  unset = "1"
)))

if (is.na(lookback_days) || lookback_days < 0) {
  lookback_days <- 1L
}

tz_local <- "America/Los_Angeles"
today_local <- as.Date(lubridate::with_tz(Sys.time(), tz_local))
start_date <- today_local - lookback_days
end_date <- today_local

start_date_chr <- format(start_date, "%Y-%m-%d")
end_date_chr <- format(end_date, "%Y-%m-%d")
feed_build_time_utc <- format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# ---- 3. Helpers -------------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_num <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub("[^0-9.-]", "", x)
  x[x == "" | x == "-" | x == "."] <- NA_character_
  suppressWarnings(as.numeric(x))
}

pt_bool <- function(x, default = FALSE) {
  raw <- trimws(tolower(as.character(x)))
  out <- raw %in% c("true", "t", "1", "yes", "y")
  missing <- is.na(x) | raw == "" | raw %in% c("na", "null", "nan")
  out[missing] <- default
  out
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

pt_fetch_text <- function(url, label = url, timeout_sec = 45, retries = 3) {
  message("Fetching ", label, ": ", url)

  user_agent <- paste(
    "Mozilla/5.0",
    "BRIM CoCoRaHS daily precip feed builder",
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
          "Accept" = "application/json,text/json,*/*;q=0.8",
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

pt_query_url <- function(state, offset) {
  params <- list(
    offset = as.character(offset),
    limit = as.character(cocorahs_limit),
    startDate = start_date_chr,
    endDate = end_date_chr,
    sortField = "obsDateTime",
    sortDir = "desc",
    country = "US",
    subdiv1 = state,
    units = cocorahs_units
  )

  paste0(cocorahs_api_url, "?", paste(
    paste0(utils::URLencode(names(params), reserved = TRUE), "=", utils::URLencode(unlist(params), reserved = TRUE)),
    collapse = "&"
  ))
}

pt_api_results <- function(parsed) {
  if (!is.null(parsed$results)) return(parsed$results)
  if (!is.null(parsed$Results)) return(parsed$Results)
  list()
}

pt_api_total_count <- function(parsed) {
  candidates <- list(
    parsed$metadata$resultset$totalCount,
    parsed$metadata$resultSet$totalCount,
    parsed$Metadata$Resultset$TotalCount,
    parsed$Metadata$ResultSet$TotalCount,
    parsed$totalCount,
    parsed$TotalCount
  )

  for (x in candidates) {
    n <- suppressWarnings(as.integer(x))
    if (!is.na(n)) return(n)
  }

  NA_integer_
}

pt_as_records <- function(x) {
  ## CoCoRaHS API results arrive as a JSON array of record objects.  When the
  ## response is parsed with simplifyVector = FALSE, that becomes a plain R
  ## list-of-lists, not a data frame.  jsonlite::flatten() only accepts data
  ## frames, so convert list records back through JSON with flatten = TRUE.
  ## This keeps the parser tolerant of both lower-camel and PascalCase API
  ## fields, and of occasional nested objects that jsonlite can flatten into
  ## dotted column names.
  if (is.null(x)) return(tibble::tibble())

  if (is.data.frame(x)) {
    return(tibble::as_tibble(jsonlite::flatten(x)))
  }

  if (is.list(x) && length(x) > 0) {
    y <- tryCatch({
      jsonlite::fromJSON(
        jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"),
        flatten = TRUE
      )
    }, error = function(e) {
      warning("Could not coerce CoCoRaHS results to a flat table: ", conditionMessage(e))
      NULL
    })

    if (is.data.frame(y)) {
      return(tibble::as_tibble(y))
    }

    ## Last-resort fallback for unusual list shapes: keep scalar fields and
    ## drop nested/list-valued fields rather than failing the whole feed build.
    rows <- lapply(x, function(rec) {
      if (!is.list(rec)) return(tibble::tibble(value = as.character(rec)))

      scalar <- rec[vapply(rec, function(v) {
        is.null(v) || length(v) <= 1 && !is.list(v)
      }, logical(1))]

      if (!length(scalar)) return(tibble::tibble())
      tibble::as_tibble(scalar)
    })

    return(dplyr::bind_rows(rows))
  }

  tibble::tibble()
}

pt_get_col <- function(df, candidates, default = NA_character_) {
  if (nrow(df) == 0) return(character())

  nms <- names(df)
  lower <- tolower(nms)

  for (candidate in candidates) {
    idx <- match(tolower(candidate), lower)
    if (!is.na(idx)) return(df[[idx]])
  }

  rep(default, nrow(df))
}

pt_add_if_present <- function(props, name, value) {
  if (length(value) == 0 || is.null(value) || is.na(value) || identical(value, "")) {
    props[[name]] <- NULL
  } else {
    props[[name]] <- value
  }
  props
}

pt_station_url <- function(station_number) {
  station_number <- pt_chr(station_number)
  if (is.na(station_number) || station_number == "") return(NA_character_)
  paste0(
    "https://www.cocorahs.org/ViewData/ViewStationPrecipSummary.aspx?StationNumber=",
    utils::URLencode(station_number, reserved = TRUE)
  )
}

pt_precip_amount <- function(precip, gauge_catch, precip_trace, gauge_trace) {
  ## Properties written to GeoJSON intentionally omit missing values as NULL.
  ## When R reads those back as list elements, p$precip or p$gaugeCatch can be
  ## NULL / length zero.  pt_num(NULL) returns numeric(0), so always check
  ## length before using is.na() in an if statement.
  if (isTRUE(precip_trace) || isTRUE(gauge_trace)) return(0.001)

  p <- pt_num(precip)
  if (length(p) > 0 && !is.na(p[1])) return(as.numeric(p[1]))

  g <- pt_num(gauge_catch)
  if (length(g) > 0 && !is.na(g[1])) return(as.numeric(g[1]))

  NA_real_
}

pt_fetch_state <- function(state) {
  offset <- 0L
  out <- list()
  total_count <- NA_integer_
  page <- 1L

  repeat {
    if (page > cocorahs_max_pages_per_state) {
      warning("Reached COCORAHS_MAX_PAGES_PER_STATE for ", state, "; stopping pagination.")
      break
    }

    url <- pt_query_url(state = state, offset = offset)
    txt <- pt_fetch_text(url, label = paste0("CoCoRaHS ", state, " page ", page))

    parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    results <- pt_api_results(parsed)
    rows <- pt_as_records(results)

    if (is.na(total_count)) {
      total_count <- pt_api_total_count(parsed)
    }

    if (nrow(rows) == 0) {
      break
    }

    rows$brim_query_state <- state
    rows$brim_query_offset <- offset
    rows$brim_query_url <- url

    out[[length(out) + 1L]] <- rows

    offset <- offset + nrow(rows)
    page <- page + 1L

    if (!is.na(total_count) && offset >= total_count) {
      break
    }

    if (nrow(rows) < cocorahs_limit) {
      break
    }

    Sys.sleep(cocorahs_request_pause_sec)
  }

  data <- if (length(out) > 0) {
    dplyr::bind_rows(out)
  } else {
    tibble::tibble()
  }

  list(
    state = state,
    rows = data,
    total_count = total_count,
    fetched_count = nrow(data)
  )
}

pt_record_features <- function(df) {
  if (nrow(df) == 0) return(list())

  station_number <- pt_chr(pt_get_col(df, c("stationNumber", "StationNumber", "station_number")))
  station_name <- pt_chr(pt_get_col(df, c("stationName", "StationName", "station_name")))
  latitude <- pt_num(pt_get_col(df, c("latitude", "Latitude", "lat", "Lat")))
  longitude <- pt_num(pt_get_col(df, c("longitude", "Longitude", "lon", "Lon", "lng", "Lng")))
  obs_datetime <- pt_chr(pt_get_col(df, c("obsDateTime", "ObsDateTime", "observationDateTime", "ObservationDateTime")))
  entry_datetime <- pt_chr(pt_get_col(df, c("entryDateTime", "EntryDateTime")))
  timestamp <- pt_chr(pt_get_col(df, c("dateTimeStamp", "DateTimeStamp")))
  precip <- pt_num(pt_get_col(df, c("precip", "Precip", "gaugeCatch", "GaugeCatch", "totalPrecipAmt", "TotalPrecipAmt", "precipDurationAmt", "PrecipDurationAmt")))
  gauge_catch <- pt_num(pt_get_col(df, c("gaugeCatch", "GaugeCatch", "precip", "Precip", "totalPrecipAmt", "TotalPrecipAmt", "precipDurationAmt", "PrecipDurationAmt")))
  precip_is_trace <- pt_bool(pt_get_col(df, c("precipIsTrace", "PrecipIsTrace", "gaugeCatchIsTrace", "GaugeCatchIsTrace", "totalPrecipAmtIsTrace", "TotalPrecipAmtIsTrace", "precipDurationAmtIsTrace", "PrecipDurationAmtIsTrace")), default = FALSE)
  gauge_catch_is_trace <- pt_bool(pt_get_col(df, c("gaugeCatchIsTrace", "GaugeCatchIsTrace", "precipIsTrace", "PrecipIsTrace", "totalPrecipAmtIsTrace", "TotalPrecipAmtIsTrace", "precipDurationAmtIsTrace", "PrecipDurationAmtIsTrace")), default = FALSE)

  depth_snowfall <- pt_num(pt_get_col(df, c("depthOfSnowfall", "DepthOfSnowfall")))
  depth_snowfall_trace <- pt_bool(pt_get_col(df, c("depthOfSnowfallIsTrace", "DepthOfSnowfallIsTrace")), default = FALSE)
  swe_snowfall <- pt_num(pt_get_col(df, c("waterContentOfSnowfall", "WaterContentOfSnowfall")))
  swe_snowfall_trace <- pt_bool(pt_get_col(df, c("waterContentOfSnowfallIsTrace", "WaterContentOfSnowfallIsTrace")), default = FALSE)
  depth_snow_ground <- pt_num(pt_get_col(df, c("depthOfSnowOnGround", "DepthOfSnowOnGround")))
  depth_snow_ground_trace <- pt_bool(pt_get_col(df, c("depthOfSnowOnGroundIsTrace", "DepthOfSnowOnGroundIsTrace")), default = FALSE)
  swe_snow_ground <- pt_num(pt_get_col(df, c("waterContentOfSnowOnGround", "WaterContentOfSnowOnGround")))
  swe_snow_ground_trace <- pt_bool(pt_get_col(df, c("waterContentOfSnowOnGroundIsTrace", "WaterContentOfSnowOnGroundIsTrace")), default = FALSE)

  flooding <- pt_chr(pt_get_col(df, c("flooding", "Flooding")))
  notes <- pt_chr(pt_get_col(df, c("notes", "Notes")))
  units <- pt_chr(pt_get_col(df, c("units", "Units")))
  source <- pt_chr(pt_get_col(df, c("source", "Source")))
  state <- pt_chr(pt_get_col(df, c("state", "State", "subdiv1", "Subdiv1", "brim_query_state")))

  id <- pt_chr(pt_get_col(df, c("id", "Id", "dailyPrecipReportID", "DailyPrecipReportID", "uid", "Uid")))

  features <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    if (is.na(latitude[i]) || is.na(longitude[i]) ||
        latitude[i] < -90 || latitude[i] > 90 ||
        longitude[i] < -180 || longitude[i] > 180) {
      features[[i]] <- NULL
      next
    }

    props <- list(
      id = if (!is.na(id[i])) as.character(id[i]) else NULL,
      stationNumber = if (!is.na(station_number[i])) station_number[i] else NULL,
      stationName = if (!is.na(station_name[i])) station_name[i] else NULL,
      latitude = latitude[i],
      longitude = longitude[i],
      obsDateTime = if (!is.na(obs_datetime[i])) obs_datetime[i] else NULL,
      entryDateTime = if (!is.na(entry_datetime[i])) entry_datetime[i] else NULL,
      dateTimeStamp = if (!is.na(timestamp[i])) timestamp[i] else NULL,
      precip = if (!is.na(precip[i])) precip[i] else NULL,
      precipIsTrace = isTRUE(precip_is_trace[i]),
      gaugeCatch = if (!is.na(gauge_catch[i])) gauge_catch[i] else NULL,
      gaugeCatchIsTrace = isTRUE(gauge_catch_is_trace[i]),
      depthOfSnowfall = if (!is.na(depth_snowfall[i])) depth_snowfall[i] else NULL,
      depthOfSnowfallIsTrace = isTRUE(depth_snowfall_trace[i]),
      waterContentOfSnowfall = if (!is.na(swe_snowfall[i])) swe_snowfall[i] else NULL,
      waterContentOfSnowfallIsTrace = isTRUE(swe_snowfall_trace[i]),
      depthOfSnowOnGround = if (!is.na(depth_snow_ground[i])) depth_snow_ground[i] else NULL,
      depthOfSnowOnGroundIsTrace = isTRUE(depth_snow_ground_trace[i]),
      waterContentOfSnowOnGround = if (!is.na(swe_snow_ground[i])) swe_snow_ground[i] else NULL,
      waterContentOfSnowOnGroundIsTrace = isTRUE(swe_snow_ground_trace[i]),
      flooding = if (!is.na(flooding[i])) flooding[i] else NULL,
      notes = if (!is.na(notes[i])) notes[i] else NULL,
      units = if (!is.na(units[i])) units[i] else cocorahs_units,
      source = if (!is.na(source[i])) source[i] else "CoCoRaHS",
      state = if (!is.na(state[i])) state[i] else NA_character_,
      stationUrl = pt_station_url(station_number[i]),
      sourceWindowStartDate = start_date_chr,
      sourceWindowEndDate = end_date_chr,
      feedBuildTimeUtc = feed_build_time_utc
    )

    features[[i]] <- list(
      type = "Feature",
      id = if (!is.na(id[i])) as.character(id[i]) else paste0(station_number[i], "_", obs_datetime[i]),
      geometry = list(
        type = "Point",
        coordinates = list(longitude[i], latitude[i])
      ),
      properties = props
    )
  }

  features <- features[!vapply(features, is.null, logical(1))]
  unname(features)
}

# ---- 4. Fetch data ----------------------------------------------------------

state_results <- purrr::map(cocorahs_states, pt_fetch_state)

rows <- purrr::map(state_results, "rows") |>
  dplyr::bind_rows()

api_total_count <- sum(vapply(state_results, function(x) {
  if (is.na(x$total_count)) return(0L)
  as.integer(x$total_count)
}, integer(1)))
api_rows_fetched <- nrow(rows)

message("CoCoRaHS API rows fetched: ", api_rows_fetched)

# Basic de-duplication.  The API can be paginated safely, but keep duplicate
# station/time/report IDs out of the map if an upstream page overlaps.
if (nrow(rows) > 0) {
  id_col <- names(rows)[tolower(names(rows)) %in% tolower(c("id", "dailyPrecipReportID", "uid"))][1]
  station_col <- names(rows)[tolower(names(rows)) %in% tolower(c("stationNumber"))][1]
  obs_col <- names(rows)[tolower(names(rows)) %in% tolower(c("obsDateTime"))][1]

  if (!is.na(id_col)) {
    rows <- rows |>
      dplyr::distinct(.data[[id_col]], .keep_all = TRUE)
  } else if (!is.na(station_col) && !is.na(obs_col)) {
    rows <- rows |>
      dplyr::distinct(.data[[station_col]], .data[[obs_col]], .keep_all = TRUE)
  }
}

features <- pt_record_features(rows)

# ---- 5. Summary counts ------------------------------------------------------

amounts <- if (length(features) > 0) {
  vapply(features, function(f) {
    p <- f$properties
    pt_precip_amount(p$precip, p$gaugeCatch, p$precipIsTrace, p$gaugeCatchIsTrace)
  }, numeric(1))
} else {
  numeric(0)
}

trace_count <- if (length(features) > 0) {
  sum(vapply(features, function(f) {
    isTRUE(f$properties$precipIsTrace) || isTRUE(f$properties$gaugeCatchIsTrace)
  }, logical(1)), na.rm = TRUE)
} else {
  0L
}

zero_count <- sum(!is.na(amounts) & amounts == 0, na.rm = TRUE)
measurable_count <- sum(!is.na(amounts) & amounts > 0, na.rm = TRUE)
missing_amount_count <- sum(is.na(amounts), na.rm = TRUE)

summary <- list(
  feed_name = "CoCoRaHS daily precipitation latest",
  feed_build_time_utc = feed_build_time_utc,
  source = "CoCoRaHS DailyPrecipObs API",
  source_api_url = cocorahs_api_url,
  states = as.list(cocorahs_states),
  country = "US",
  units = cocorahs_units,
  timezone_for_date_window = tz_local,
  start_date = start_date_chr,
  end_date = end_date_chr,
  lookback_days = lookback_days,
  schedule_note = "GitHub workflow is intended to run about every 8 hours.",
  api_total_count = api_total_count,
  api_rows_fetched = api_rows_fetched,
  output_feature_count = length(features),
  measurable_count = measurable_count,
  zero_count = zero_count,
  trace_count = trace_count,
  missing_amount_count = missing_amount_count,
  max_precip_in = if (length(amounts) && any(!is.na(amounts))) max(amounts, na.rm = TRUE) else NA_real_,
  query = list(
    page_limit = cocorahs_limit,
    max_pages_per_state = cocorahs_max_pages_per_state,
    request_pause_sec = cocorahs_request_pause_sec
  ),
  caveat = "Volunteer-reported daily observations; use as supplemental screening and storm-verification context."
)

geojson <- list(
  type = "FeatureCollection",
  name = "cocorahs_daily_precip_latest",
  metadata = summary,
  features = features
)

# ---- 6. Write outputs -------------------------------------------------------

dir.create(dirname(out_geojson), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_summary), recursive = TRUE, showWarnings = FALSE)

jsonlite::write_json(
  geojson,
  path = out_geojson,
  auto_unbox = TRUE,
  pretty = FALSE,
  null = "null",
  na = "null"
)

jsonlite::write_json(
  summary,
  path = out_summary,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  na = "null"
)

message("Wrote GeoJSON: ", out_geojson)
message("Wrote summary: ", out_summary)
message("Output features: ", length(features))
