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
  dplyr::arrange(.data$cdec_id)

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(idx_tbl, out_csv)

message("Exported BRIM CDEC reservoir station index:")
message("  rows: ", nrow(idx_tbl))
message("  path: ", out_csv)
