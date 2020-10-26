
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

Authorise a Google Identity to access The Lab’s cloud infrastructure by
doing:

``` r
# install and load required packages inc. dependencies  , eval=FALSE, echo=TRUE
if(!require("pacman")) install.packages("pacman")
#> Loading required package: pacman
pacman::p_load_gh("RWLab/rwRtools", dependencies = TRUE)

rwlab_gc_auth()
#> Using an auto-discovered, cached token.
#> To suppress this message, modify your code or options to clearly consent to the use of a cached token.
#> See gargle's "Non-interactive auth" vignette for more details:
#> https://gargle.r-lib.org/articles/non-interactive-auth.html
#> The gargle package is using a cached token for kris@robotwealth.com.
#> i 2020-10-26 12:44:34 > Setting client.id from options(googleAuthR.client_id)
```

You will be prompted to select a Google Identity in a browser and the
copy and paste an authentication code back at the call site.
