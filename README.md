
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwRtools

<!-- badges: start -->

<!-- badges: end -->

The goal of rwRtools is to make it easy to access The Lab’s datasets and
get started with research.

TODO: big up the lab, links to RW etc

## Installation

You can install rwRtools from GitHub like so:

``` r
devtools::install_github("RWLab/rwRtools", dependencies = TRUE)
```

## Demo

### Install and load

The easiest way to install and load rwRtools and its dependencies is via
the `pacman` library:

``` r
if(!require("pacman")) install.packages("pacman")
#> Loading required package: pacman
pacman::p_load_gh("RWLab/rwRtools", dependencies = TRUE)
```

### Quickstart: Set up for working on a Research Pod

After installing and loading rwRtools, this is the quickest way to set
up a session for working on a particular Research Pod.

``` r
setup_for_pod(pod = "EquityFactors", path = ".")
```

This kicks off the OAuth process and if successful, transfers all
objects associated with the Pod from GCS to `path`.

If your session is interactive, you will be prompted in a browser to
select a Google Identity and copy and paste an authentication code back
at the call site.

### List The Lab’s Research Pods

Each Pod has an associated GCS bucket containing datasets and other
objects relevant to the Pod. Get a list of currently available Pods by
doing:

``` r
list_pods()
#> [1] "EquityFactors"
```

This function does not access GCS and therefore does not require an
authorisation step.

### See a Research Pod’s GCS objects

See all objects associated with a Research Pod:

``` r
get_pod_meta(pod = "EquityFactors")
#> $bucket
#> [1] "rw_equity_research_sprint"
#> 
#> $datasets
#> [1] "clean_R1000.csv"  "fundamentals.csv"
```

This function does not access GCS and therefore does not require an
authorisation step.

### Kick off an OAuth process to access The Lab’s cloud infrastructure

If called in an interactive session, you will be prompted in a browser
to select a Google Identity and copy and paste an authentication code
back at the call site.

This is useful if you need re-authorise, or if you want to access
specific Lab objects in GCS without doing the entire setup process.

``` r
rwlab_gc_auth()
#> Using an auto-discovered, cached token.
#> To suppress this message, modify your code or options to clearly consent to the use of a cached token.
#> See gargle's "Non-interactive auth" vignette for more details:
#> https://gargle.r-lib.org/articles/non-interactive-auth.html
#> The gargle package is using a cached token for kris@robotwealth.com.
#> i 2020-10-26 13:31:32 > Setting client.id from options(googleAuthR.client_id)
```

### Load all GCS objects for a Research Pod

This transfers all objects associated with a Pod from GCS to `path`,
overwriting any existing local Pod objects.

This is useful if you need a fresh copy of the Pod’s datasets, but don’t
need to re-authorise to GCS. Requires that you’ve already authorised to
the relevant GCS bucket.

``` r
load_pod_data(pod = "EequityFactors", path = ".")
```

### Load a specific GCS object

This transfers a specifc object from GCS to `path`, overwriting any
existing local instance of that object.

This is useful if you need a fresh copy of a single dataset, but don’t
need to re-authorise to GCS. Requires that you’ve already authorised to
the relevant GCS bucket.

``` r
load_lab_object(path = ".", object = "clean_R1000.csv", bucket = "rw_equity_research_sprint")
```
