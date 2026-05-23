# ==== export_brim_cdec_station_index.R =======================================
#
# PURPOSE:
#   Export BRIM's CDEC reservoir station index to a lightweight CSV that can be
#   committed into the separate brim-live-reservoir-feed GitHub repository.
#
# HOW TO RUN:
#   Run this script from the BRIM project root after running:
#
#     source("02_preprocess/28_reservoir_station_index.r")
#
#   By default, the script writes to a sibling folder named:
#
#     ../brim-live-reservoir-feed/data/input/cdec_reservoir_station_index.csv
#
#   You can override the target repository folder by setting:
#
#     Sys.setenv(BRIM_LIVE_FEED_REPO = "C:/path/to/brim-live-reservoir-feed")
#
# DESIGN:
#   The station index remains a static metadata/crosswalk backbone.  Current
#   storage values are fetched separately by build_cdec_reservoir_latest.R.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(tibble)
})

source("00_config/config_paths.r")

feed_repo <- Sys.getenv(
  "BRIM_LIVE_FEED_REPO",
  unset = normalizePath(file.path(DIR$root, "..", "brim-live-reservoir-feed"), mustWork = FALSE)
)

in_rds <- file.path(DIR$rds, "cdec_reservoir_station_index_wgs84.rds")
out_csv <- file.path(feed_repo, "data", "input", "cdec_reservoir_station_index.csv")

if (!file.exists(in_rds)) {
  stop("CDEC reservoir station index RDS not found: ", in_rds,
       "\nRun source('02_preprocess/28_reservoir_station_index.r') first.")
}

idx <- readRDS(in_rds)

# ---- Optional CDEC-CNRFC crosswalk enrichment -------------------------------
#
# The CDEC reservoir station index is still the CDEC-first metadata backbone.
# This optional join only adds the best current BRIM CDEC↔CNRFC/NWS crosswalk
# where available, so the live hosted feed can show CNRFC reservoir links for
# major reservoirs such as SHA→SHDC1 and FOL→FOLC1.
#
# If the crosswalk file is missing, the export still succeeds using the
# station-index fields only.

xwalk_rds <- file.path(DIR$rds, "cdec_cnrfc_station_crosswalk.rds")

if (file.exists(xwalk_rds)) {

  message("Reading optional CDEC-CNRFC station crosswalk: ", xwalk_rds)

  xwalk_raw <- readRDS(xwalk_rds)

  if (!"match_confidence" %in% names(xwalk_raw)) {
    xwalk_raw$match_confidence <- NA_character_
  }

  if (!"match_method" %in% names(xwalk_raw)) {
    xwalk_raw$match_method <- NA_character_
  }

  xwalk_tbl <- xwalk_raw |>
    dplyr::mutate(
      cdec_id = toupper(trimws(as.character(.data$cdec_id))),
      cnrfc_nws_id = toupper(trimws(as.character(.data$nws_id))),
      cnrfc_match_confidence = as.character(.data$match_confidence),
      cnrfc_match_method = as.character(.data$match_method),
      cnrfc_match_rank = dplyr::case_when(
        tolower(.data$cnrfc_match_confidence) == "high" ~ 1L,
        tolower(.data$cnrfc_match_confidence) == "medium" ~ 2L,
        tolower(.data$cnrfc_match_confidence) == "low" ~ 3L,
        TRUE ~ 9L
      )
    ) |>
    dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
    dplyr::filter(!is.na(.data$cnrfc_nws_id), .data$cnrfc_nws_id != "") |>
    dplyr::arrange(.data$cdec_id, .data$cnrfc_match_rank, .data$cnrfc_nws_id) |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE) |>
    dplyr::select(
      dplyr::any_of(c(
        "cdec_id",
        "cnrfc_nws_id",
        "cnrfc_match_confidence",
        "cnrfc_match_method"
      ))
    )

} else {

  message("Optional CDEC-CNRFC station crosswalk not found; CNRFC live-feed links will use station-index aliases only.")

  xwalk_tbl <- tibble::tibble(
    cdec_id = character(),
    cnrfc_nws_id = character(),
    cnrfc_match_confidence = character(),
    cnrfc_match_method = character()
  )
}

idx_tbl <- idx |>

  sf::st_drop_geometry() |>
  dplyr::mutate(
    cdec_id = toupper(trimws(as.character(.data$cdec_id)))
  ) |>
  dplyr::select(
    dplyr::any_of(c(
      "cdec_id",
      "reservoir_name",
      "cdec_station_name",
      "elevation_ft",
      "latitude",
      "longitude",
      "county",
      "operator_agency",
      "river_basin_cdec",
      "has_hourly_reservoir_report",
      "has_daily_reservoir_report",
      "cdec_metadata_source",
      "coord_source",
      "cdec_station_url",
      "cdec_sensor15_hourly_url",
      "cdec_sensor15_daily_url",
      "cdec_latest_storage_table_url",
      "legacy_nws_ids",
      "nws_id",
      "alias_names",
      "cnrfc_reservoir_inflow_url",
      "cnrfc_reservoir_outflow_url",
      "map_geometry_ok",
      "data_quality_note"
    ))
  ) |>
  dplyr::filter(.data$map_geometry_ok %in% TRUE) |>
  dplyr::left_join(xwalk_tbl, by = "cdec_id") |>
  dplyr::mutate(
    station_index_nws_id = toupper(trimws(as.character(.data$nws_id))),
    station_index_nws_id = dplyr::na_if(.data$station_index_nws_id, ""),
    cnrfc_nws_id = dplyr::na_if(.data$cnrfc_nws_id, ""),
    cnrfc_nws_id = dplyr::coalesce(.data$cnrfc_nws_id, .data$station_index_nws_id),
    nws_id = dplyr::coalesce(.data$cnrfc_nws_id, .data$station_index_nws_id)
  ) |>
  dplyr::arrange(.data$cdec_id)

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(idx_tbl, out_csv)

message("Exported BRIM CDEC reservoir station index:")
message("  rows: ", nrow(idx_tbl))
message("  rows with CNRFC/NWS ID: ", sum(!is.na(idx_tbl$cnrfc_nws_id) & idx_tbl$cnrfc_nws_id != ""))
message("  path: ", out_csv)
