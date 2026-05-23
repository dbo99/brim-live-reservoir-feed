# BRIM live reservoir feed prototype

This small repository builds a near-live/static GeoJSON feed for BRIM reservoir storage.

The intended workflow is:

1. Export the BRIM CDEC reservoir station index to `data/input/cdec_reservoir_station_index.csv`.
2. Run `scripts/build_cdec_reservoir_latest.R` locally and confirm it creates:
   - `docs/data/cdec_reservoir_latest.geojson`
   - `docs/data/cdec_reservoir_latest_summary.json`
3. Push the repository to GitHub.
4. Enable GitHub Pages from the `docs/` folder on the `main` branch.
5. Enable the included GitHub Actions workflow so the GeoJSON is refreshed on a schedule.

This repository is intentionally separate from the full BRIM project so live-feed testing does not risk the main standalone Leaflet build.

## Current feed

`docs/data/cdec_reservoir_latest.geojson`

Near-live/provisional CDEC reservoir storage, joined to the BRIM CDEC reservoir station index. File names use `latest`, not `live`, because the feed is scheduled/static and each feature carries observation/build timestamps for staleness checks.

## Static input

`data/input/cdec_reservoir_station_index.csv`

This file is exported from BRIM and should be committed to the repository so GitHub Actions can build the feed without access to the full local BRIM project.

## Future compatible feed names

The same pattern can later support related BRIM feeds:

- `docs/data/reservoirs/cdec_reservoir_storage_latest.geojson`
- `docs/data/snow/snow_pillow_swe_latest.geojson`
- `docs/data/soil/scan_soil_moisture_latest.geojson`

For now, the active output remains `docs/data/cdec_reservoir_latest.geojson` to avoid unnecessary path churn while the reservoir feed is being proven.

## Backlog notes

Possible later enrichment:

- add static `capacity_af` and `pct_capacity` using a trusted static capacity lookup;
- add a USACE California reservoir plots link for reservoirs represented on the USACE plots page;
- compute `storage_change_24h_af`, and later possibly 3-day and 10-day change, once the latest-feed workflow is stable.
