# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

rwRtools is an R package giving RW Pro members access to Robot Wealth's Lab datasets in Google Cloud Storage (GCS). It is the data access layer for 5 Research Pods: EquityFactors, FX, Crypto, Macro, and TLAQ. Each pod has its own GCS bucket and a family of `pod_get_*()` loader functions.

Members install straight from GitHub `master` (`pacman::p_load_current_gh("RWLab/rwRtools")`), so `master` is production. Bump `Version` in `DESCRIPTION` when shipping a change.

## Development Commands

```r
devtools::load_all()      # Load package locally
devtools::document()      # Regenerate man/ and NAMESPACE from roxygen2 comments (required after any @export change)
devtools::check()         # Full R CMD check
devtools::test()          # Runs nothing: tests/testthat.R has test_check() commented out and there is no tests/testthat/ dir
```

R is not on PATH in the shell. Run scripts with the full path:

```bash
"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" -e 'devtools::document()'
```

Real verification requires GCS auth (`rwlab_data_auth()` opens a browser), so most roxygen examples are `\dontrun{}` and there is no automated test coverage. Test loaders interactively.

## Architecture

### Two layers

1. **GCS core (`R/rwlab_gcs.R`)** — `get_pod_meta()` holds the hardcoded pod → bucket mapping. `transfer_lab_object()` (internal) downloads one object; `load_lab_object()` (exported) downloads and reads it as feather. `transfer_pod_data()` / `quicksetup()` bulk-load a pod's `essentials` and assign a `prices` data.frame into `.GlobalEnv`.
2. **Pod loaders (`R/crypto_data_utils.R`, `R/fx_data_utils.R`, `R/macro_pod_utils.R`, `R/equity_factors_utils.R`)** — thin wrappers that call `transfer_lab_object()` for a specific object, read it, fix column types, and return a tibble. Every loader follows the same shape:

```r
pod_get_thing <- function(path = "<pod-dir>", force_update = TRUE) {
  if(!file.exists(file.path(path, "thing.feather")) || force_update == TRUE) {
    transfer_lab_object(pod = "Pod", object = "gcs/prefix/thing.feather", path = path)
  }
  rw_read_feather(file.path(path, "thing.feather")) %>% dplyr::mutate(date = lubridate::as_date(date))
}
```

Adding a dataset = add a loader in the right pod file following this shape, `@export` it, run `devtools::document()`. Adding a pod = add an entry to `get_pod_meta()` plus a new `*_utils.R` file.

### All feather reads go through `rw_read_feather()`

`R/feather_io.R` defines the only feather reader. It wraps `arrow::read_feather()` with `arrow.use_altrep = FALSE` set locally (restored on exit). ALTREP-backed columns from arrow have crashed downstream `dplyr::filter()` calls with `vec_slice_altrep(): negative length vectors are not allowed` on large frames. The `feather` package is deprecated and is no longer a dependency. Never call `arrow::read_feather()` or `feather::read_feather()` directly in package code.

### Caching: `force_update` vs `force`

Two flags with different meanings:

- **`force_update`** (pod loaders, default `TRUE`): whether to call `transfer_lab_object()` at all. `FALSE` uses the local file with no network call.
- **`force`** (`transfer_lab_object()` / `load_lab_object()`, default `FALSE`): whether to skip the cache check. When `FALSE` and a local file exists, it fetches GCS object metadata and reuses the local file only if size matches and local mtime >= remote `updated`. Otherwise it re-downloads with overwrite.

Pod loaders do not pass `force` through, so `force_update = TRUE` means "verify against GCS and download if changed", not "always re-download". `transfer_pod_data()` (and therefore `quicksetup()`) bypasses the cache and always downloads.

### Local file layout

GCS objects may have directory prefixes (`statarb/spreads.feather`, `feather/Daily/EURUSD.feather`). Locally only the basename is kept, inside a per-pod default directory relative to the working directory:

| Pod | Default `path` |
|---|---|
| Macro | `macropod` |
| EquityFactors | `equityfactors` |
| Crypto | `binance`, `ftx`, `coinmetrics`, `coincodex` (by source) |
| FX | `Daily`, `Hourly`, `Zorro-Assets-Lists`, `Policy-Rates` |
| core `rwlab_gcs.R` functions | `.` |

Because prefixes are dropped, `equity_get_statarb_prices()` (`statarb/prices.feather`) and `equity_get_liquid_universe()` (`equity_factors/prices.feather`) both write `equityfactors/prices.feather` and will clobber each other. Pass distinct `path` values when using both.

### Pod metadata is not a dataset catalogue

The `datasets` vector in `get_pod_meta()` is stale and incomplete. Only `essentials` and `prices` are used (by `transfer_pod_data()` / `quicksetup()`). Many loaders fetch objects not listed there. FX has `NA` essentials and cannot be bulk-loaded.

`quicksetup()` reads the prices file via feather, or via hardcoded branches for two CSV names (`coinmetrics.csv`, `main_asset_classes_daily_ohlc.csv`). A new pod with a CSV prices file needs a new branch there.

### Auth (`R/rwlab_lab_auth.R`)

`rwlab_data_auth()` takes no arguments (the `oauth_email` argument shown in README.md and older docs no longer exists). It uses gargle's built-in web OAuth client via unexported internals (`gargle:::goc_web()`, `gargle:::is_google_colab()`), forces OOB auth, and hands the token to `googleCloudStorageR::gcs_auth()`. This is known to be fragile across gargle versions. `R/app.R` (`get_lab_app()` calling an undefined `gla()`) and `R/sysdata.rda` (an OAuth client `json_string`) are remnants of a previous auth approach and are unused.

## Code Conventions

- **Naming:** `pod_get_noun_timeframe()`, e.g. `crypto_get_binance_spot_1d()`, `macro_get_expiring_vx_futures()`, `fx_get_daily_OHLC()`.
- **Namespace:** `NAMESPACE` imports only `%>%`. Everything else must be fully qualified (`glue::glue()`, `readr::read_csv()`, `lubridate::as_date()`). Existing loaders call `mutate`, `filter`, `select`, `arrange`, `rename` unqualified and only work because members have dplyr attached. Use `dplyr::` in new code; R CMD check flags the unqualified ones.
- **Paths:** build with `file.path(path, ...)` or `glue::glue("{path}/...")`; both are used.
- **Output:** transfer functions report progress with `cat()`, not `message()`. Loaders return a tibble, usually arranged by date then ticker.
- **Roxygen:** every exported function has roxygen2 docs with a `\dontrun{}` example. `man/` and `NAMESPACE` are generated; never hand-edit.

## Data Conventions

- Hyperliquid timestamps are at bar close; Binance timestamps are at bar open — lag HL by one bar to align.
- FTX datasets are historical only (exchange defunct).
- Date columns should be type `Date` via `lubridate::as_date()`; datetime columns via `lubridate::as_datetime()`.
- FX loaders strip `/` from tickers (`EUR/USD` → `EURUSD`) because Zorro asset lists use slashes but GCS filenames do not.

## Colab Integration

`examples/colab/load_libraries.R` is sourced from the raw GitHub URL inside Colab. It writes and runs a bspm/r2u apt setup script, installs binaries via `pacman`, then installs rwRtools from GitHub with `dependencies = FALSE`. Any new package dependency must also be added to its `core_packages` vector or Colab installs will break. GitHub caches raw content for ~5 minutes after a push.

```r
source('https://raw.githubusercontent.com/RWLab/rwRtools/master/examples/colab/load_libraries.R')
load_libraries(load_rsims = FALSE, extra_libraries = c('patchwork'))
```

## Local-only Files

`feature-requests/` is gitignored and holds informal specs for new loader functions. `.claude/settings.local.json` is also local.
