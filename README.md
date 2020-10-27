
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwRtools

<!-- badges: start -->

<!-- badges: end -->

The goal of `rwRtools` is to make it easy to access The Lab’s datasets
and get started with research.

## What is The Lab?

The Lab is [Robot Wealth’s](https://robotwealth.com/) portal for
collaborative research.

It is organized around **Research Pods,** which contain data, ideas,
research and peer-reviewed edges for a given market question.

For example:

  - in the *Equity Factor Research Pod* we look at the question: "*What
    factors predict the relative performance of stocks in the Russell
    1000 index?"*
  - In the *Global Risk Premia Research Pod* we look at the question:
    “*What is the most effective way to get paid for taking on global
    market risks?*”

## What is the purpose of The Lab?

**The Lab serves three purposes:**

1.  It gets you hands-on with the research effort. As well as
    contributing, you’ll learn a ton in the process.
2.  It scales the research effort by enabling community contribution.
3.  It makes the fruits of that scaled research effort available to the
    entire Robot Wealth community.

## Install and load

The easiest way to install and load `rwRtools` and its dependencies is
via the `pacman` library:

``` r
if(!require("pacman")) install.packages("pacman")
#> Loading required package: pacman
pacman::p_load_current_gh("RWLab/rwRtools", dependencies = TRUE)
#> 
#>          checking for file 'C:\Users\Kris\AppData\Local\Temp\RtmpOGI4JU\remotesa420428e1558\RWLab-rwRtools-90d81af/DESCRIPTION' ...  v  checking for file 'C:\Users\Kris\AppData\Local\Temp\RtmpOGI4JU\remotesa420428e1558\RWLab-rwRtools-90d81af/DESCRIPTION' (343ms)
#>       -  preparing 'rwRtools':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   v  checking DESCRIPTION meta-information
#>       -  checking for LF line-endings in source and make files and shell scripts
#>       -  checking for empty or unneeded directories
#>       -  building 'rwRtools_0.0.0.9001.tar.gz'
#>      
#> 
```

## Quickstart: Set up for working on a Research Pod

After installing and loading `rwRtools`, the quickest way to set up a
session for working on a particular Research Pod is:

### 1\. Authorise to the data library

``` r
rwlab_data_auth(oauth_email = "your-email@somewhere.com")
```

`oauth_email` is the email address you use to log in to Robot Wealth’s
resources.

If called in an interactive session, you will be prompted in a browser
to select a Google Identity and copy and paste an authentication code
back at the call site.

### 2\. List The Lab’s Research Pods

``` r
list_pods()
#> [1] "EquityFactors"
```

### 3\. Load essential Pod data

``` r
quicksetup(pod = "EquityFactors", path = ".")
```

This transfers the essential data that you always need to `path` (ohlc,
metadata), overwriting any existing local Pod objects.

Requires that you’ve already authorised to the relevant GCS bucket.

### 4\. See all data objects associated with a Pod

``` r
get_pod_meta(pod = "EquityFactors")
#> $bucket
#> [1] "equity_factors_research_pod"
#> 
#> $datasets
#> [1] "R1000_ohlc_1d.feather"        "R1000_fundamental_1d.feather"
#> 
#> $essentials
#> [1] "R1000_ohlc_1d.feather"
```

This outputs a list of all the data objects you can transfer for a Pod.

### 5\. Load specific additional Pod data objects

``` r
load_lab_object(pod = "EquityFactors", object = "R1000_ohlc_1d.feather", path = ".")
```

This transfers a specifc object to `path`, overwriting any existing
local instance of that object.

Requires that you’ve already authorised to the relevant data library

## Examples

See [the examples](examples/) for more.
