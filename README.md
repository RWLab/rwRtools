
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwtools

<!-- badges: start -->

<!-- badges: end -->

The goal of rwtools is to make it easy to access The Lab’s datasets and
get started with research.

## Installation

You can install rwtools from GitHub like so:

``` r
devtools::install_github("RWLab/rwtools")
```

## Demo

Authorise a Google Identity to access The Lab’s cloud infrastructure by
doing:

``` r
library(rwtools)
rwlab_gc_auth("your_email@somewhere.com")
```

You will be prompted to select a Google Identity in a browser and the
copy and paste an authentication code back at the call site.
