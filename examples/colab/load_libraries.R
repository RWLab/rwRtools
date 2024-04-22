# Load libraries

"
This function is intended to be sourced directly from a github raw URL as an efficient way
to install required packages without having to load any package or wrangle large
code snippets.

This is an efficient way to:
-install necessary packages (both those that you will use directly, and those that
are dependencies, excluding pre- installed libraries)
-load the ones you want to use in your session

This snippet works by installing as many packages and dependencies using binaries
from Posit's package manager as possible, which is much faster than compiling
from source.

If not loading rsims, this should take around 35 seconds. If loading rsims, it
should take around 45s.

User Instructions:
1. Source the function from it's github raw URL, for example:
source('https://raw.githubusercontent.com/RWLab/rwRtools/master/examples/colab/load_libraries.R')
2. Call the function with the following arguments:
  a. If you don't need rsims, set load_rsims = FALSE
  b. If you want to load additional libraries, specify them as a character vector
  and pass to extra_libraries:
  extra_libraries = c('patchwork', 'slider')
  c. If you know the dependencies of these libraries, you can pass them to extra_dependencies:
  extra_dependencies = c('grid', 'graphics')

How to figure out a package's dependencies? Use available.packages(). Example for
getting the packages that patchwork depends upon:
pkgs <- available.packages()
pkgs['patchwork','Imports']

If you don't know the dependencies of your extra packages, don't worry. They'll
be automatically installed, just a little slower than if you specified them.

Also don't worry about duplicating package installs. This is handled by the function.

If you get stuck, ask on Slack!

Note for devs:
GitHub caches raw content for 5 minutes, so any changes will take 5 minutes to
show up in GitHub.

TODO: make a debug message with status and return it
"
load_libraries <- function(load_rsims = TRUE, extra_libraries = c(), extra_dependencies = c()) {
  # set options to favour binaries from Posit Package Manager
  options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
  options(download.file.extra = sprintf("--header \"User-Agent: R (%s)\"", paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
  options(repos = c(REPO_NAME = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))
  options(Ncpus = 2)  # 2 cores in standard colab... might as well use them
  cat("Using", getOption("Ncpus", 1L), " CPUs for package installation")

  install.packages(c("broom", "conflicted", "dbplyr", "dplyr", "dtplyr", "forcats", "ggplot2", "googledrive", "googlesheets4", "haven", "hms", "httr",
"jsonlite", "lubridate", "magrittr", "modelr", "pillar", "purrr",
"ragg", "readr", "readxl", "reprex", "rlang", "rvest", "stringr",
"tibble", "tidyr", "xml2"))
  tidyverse::tidyverse_update()

  # install pacman the old fashioned way - isn't listed as an ubuntu package
  install.packages('pacman')

  # rwRtools dependencies (install but don't load)
  rwRtools_dependencies <- c(
    "pillar", "httr", "iterators", "zoo", "R.methodsS3",
    "callr", "foreach", "xts", "stringi", "Rcpp", "R.oo", "gargle", "assertthat",
    "googleAuthR", "glue", "googleCloudStorageR", "R.utils", "feather", "arrow",
    "TTR", "doParallel"
  )

  # libraries to load (install and load)
  libs_to_load <- c(
    c("tidyverse", "lubridate"),
    extra_libraries
  )

  # dependencies (install but don't load)
  other_dependencies <- c(
    "generics", "lifecycle", "R6", "vctrs", "pillar",
    "ellipsis", "digest", "gtable", "isoband", "MASS", "mgcv", "scales", "withr",
    "stringi", "iterators", "R.methodsS3", "openssl", "foreach", "xts",
    "R.oo", "RcppArmadillo", "slam", "timeDate", "cccp", "Rglpk", "timeSeries",
    "here", "roll", "Rcpp", "RcppParallel"
  )

  # libraries to install
  to_install <- unique(c(
    libs_to_load,
    other_dependencies,
    rwRtools_dependencies,
    extra_dependencies
  ))

  # pre-installed libraries
  installed <- installed.packages()[, "Package"]

  # remove from apt list any packages that are already installed
  to_install <- to_install[!to_install %in% installed]

  # remove arrow and install v13 instead (latest takes forever)
  to_install <- to_install[to_install != "arrow"]

  # install
  devtools::install_version('arrow', '13.0.0.1')
  install.packages(to_install, dependencies = FALSE)


  tryCatch({
    # set to TRUE will catch any missed dependencies
    # remove tidyverse from libs to load and load separately with library
    # load tidyverse first - ensures necessary packages aren't overwritten
    require(tidyverse)
    pacman::p_load(char = libs_to_load[libs_to_load != "tidyverse"], install = TRUE)

    # install and load rwRtools from GH (sans dependencies)
    pacman::p_load_current_gh("RWLab/rwRtools", dependencies = FALSE)

    # install and load rsims from GH (sans dependencies)
    if(load_rsims == TRUE)
      pacman::p_load_current_gh("Robot-Wealth/rsims", dependencies = FALSE)
  }, error = function(e) {
    print(e)
  })
}
