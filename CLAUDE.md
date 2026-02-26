# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

rwRtools is an R package providing access to Robot Wealth's Lab datasets stored in Google Cloud Storage (GCS). It serves as the data access layer for 5 Research Pods: EquityFactors, FX, Crypto, Macro, and TLAQ. Each pod has its own GCS bucket and specialized data loader functions.

## Development Commands

```r
devtools::load_all()      # Load package locally for testing
devtools::document()      # Regenerate man/ pages and NAMESPACE from roxygen2 comments
devtools::check()         # Full R CMD check
devtools::test()          # Run testthat tests (currently minimal)
```

The RStudio project (`rwRtools.Rproj`) is configured with `BuildType: Package` and `PackageUseDevtools: Yes`.

## Architecture

### Source Files (R/)

- **rwlab_gcs.R** — Core pod metadata and GCS access. Contains hardcoded metadata mapping pod names to bucket names, dataset lists, and essential datasets. Functions: `list_pods()`, `get_pod_meta()`, `quicksetup()`, `load_lab_object()`, `transfer_lab_object()`, `transfer_pod_data()`.
- **rwlab_lab_auth.R** — OAuth2 authentication via `gargle`/`googleAuthR`. Detects Google Colab and sets OOB auth. Passes token to `googleCloudStorageR`.
- **crypto_data_utils.R** — 20+ functions loading Binance spot/perps/funding, Coinmetrics, Coincodex, and historical FTX data from the `crypto_research_pod` bucket.
- **fx_data_utils.R** — FX data loaders, Zorro asset list parsing, currency conversion, policy rates, and total return index calculation from the `fx_research_pod` bucket.
- **macro_pod_utils.R** — Macro data loaders for asset classes, futures (including VIX), rates, bonds, earnings, short sale data from the `macro_research_pod` bucket.
- **app.R** — OAuth app helper (`get_lab_app()`).
- **utils-pipe.R** — Re-exports magrittr `%>%`.

### Data Loader Pattern

All pod-specific data functions follow the same pattern:
1. Check if file exists locally at `path` (or `force_update == TRUE`)
2. Download from GCS via `googleCloudStorageR::gcs_get_object()` if needed
3. Read file (CSV via `readr`, feather via `feather`/`arrow`)
4. Parse dates with `lubridate`, arrange/rename columns
5. Return as tibble

Default `force_update = TRUE` means data is always re-downloaded unless explicitly set to `FALSE`.

### Pod Metadata (Hardcoded in rwlab_gcs.R)

Pod metadata is defined inline as a list structure mapping each pod to its GCS bucket name, list of available datasets, essential datasets (loaded by `quicksetup()`), and the prices object name. Adding a new pod requires editing this structure and adding corresponding data loader functions.

### GCS Bucket Names

- `equity_factors_research_pod`
- `fx_research_pod`
- `crypto_research_pod`
- `macro_research_pod`
- `tlaq_public`

## Function Naming Convention

`pod_verb_noun_timeframe()` — e.g., `crypto_get_binance_spot_1d()`, `macro_get_expiring_vx_futures()`, `fx_get_daily_OHLC()`.

## Documentation

All 48 exported functions are documented with roxygen2 comments in the source files. Run `devtools::document()` to regenerate `man/` and `NAMESPACE`. Most examples use `\dontrun{}` because they require GCS authentication.

## Key Dependencies

- **GCS access:** `googleCloudStorageR`, `gargle`, `googleAuthR`
- **Data I/O:** `feather`, `arrow`, `readr`
- **Data manipulation:** `dplyr`, `purrr`, `stringr`, `lubridate`, `magrittr`, `zoo`

## Data Conventions

- Hyperliquid timestamps are at bar close; Binance timestamps are at bar open — lag HL by one bar to align.
- FTX datasets are historical only (exchange defunct).
- Files with directory structure in GCS (e.g., `feather/Daily/EURUSD.feather`) are saved locally with just the filename.
- Date columns should be type `Date` (use `lubridate::as_date()`).

## Colab Integration

`examples/colab/load_libraries.R` provides a one-line setup for Google Colab that installs dependencies via bspm/apt for speed, then loads rwRtools. Used as:
```r
source('https://raw.githubusercontent.com/RWLab/rwRtools/master/examples/colab/load_libraries.R')
load_libraries(load_rsims = FALSE, extra_libraries = c('patchwork'))
```
