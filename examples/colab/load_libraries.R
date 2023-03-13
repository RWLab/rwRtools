# Load libraries

"
This function is intended to be sourced directly from a github raw URL as an efficient way
to install required packages without having to load any package or wrangle large
code snippets.

This is an efficient way to:
-install necessary packages (both those that you will use directly, and those that
are dependencies, excluding pre- installed libraries)
-load the ones you want to use in your session

This snippet works by installing as many packages and dependencies using `apt-get`
as possible, which is much faster than using install.packages()

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

IF SOMETHING GOES WRONG:
Debug the install process by inspecting the msg2 object, which holds the output
of the apt install process. This object is returned invisibly by the function,
so you would do:
debug <- load_libraries()
cat(debug)

If you get stuck, ask on Slack!

Note for devs:
GitHub caches raw content for 5 minutes, so any changes will take 5 minutes to
show up in GitHub.
"

load_libraries <- function(load_rsims = TRUE, extra_libraries = c(), extra_dependencies = c()) {
  # install pacman the old fashioned way - isn't listed as an ubuntu package
  install.packages('pacman')

  # rwRtools dependencies (install but don't load)
  rwRtools_dependencies <- c(
    "pillar", "tibble", "rlang", "httr", "iterators", "zoo", "R.methodsS3",
    "callr", "foreach", "xts", "stringi", "Rcpp", "R.oo", "gargle",
    "googleAuthR", "glue", "googleCloudStorageR", "R.utils", "feather",
    "lubridate", "readr", "stringr", "dplyr", "purrr", "magrittr", "TTR", "doParallel"
  )

  # libraries to load (install and load)
  libs_to_load <- c(
    c("tidyverse", "glue"),
    extra_libraries
  )

  # dependencies (install but don't load)
  other_dependencies <- c(
    "generics", "lifecycle", "R6", "rlang", "tidyselect", "vctrs", "pillar",
    "ellipsis", "digest", "gtable", "isoband", "MASS", "mgcv", "scales", "withr",
    "stringi", "iterators", "R.methodsS3", "openssl", "foreach", "xts",
    "R.oo", "RcppArmadillo", "slam", "timeDate", "cccp", "Rglpk", "timeSeries",
    "tibble", "tidyr", "here", "roll", "Rcpp", "RcppParallel"
  )

  # pre-installed libraries
  installed <- installed.packages()[, "Package"]

  # libraries to install from CRAN
  install_from_cran <- unique(c(
    libs_to_load,
    other_dependencies,
    rwRtools_dependencies,
    extra_dependencies
  ))
  install_from_cran <- install_from_cran[!install_from_cran %in% installed]

  # convert to lowercase all letters other than an "R" at the start followed by "."
  # capitalisation of call to CRAN may not always match capitalisation of
  # package name (eg library(doParallel) vs sudo apt install r-cran-doparallel)
  install_from_cran <- gsub("^(?!R\\.)([\\w]+)", "\\L\\1", install_from_cran, perl = TRUE)

  msg1 <- system2('sudo', args = c('apt-get', 'update'),
                  stdout = TRUE,
                  stderr = TRUE,
                  wait = TRUE
  )

  msg2 <- system2(
    'sudo',
    args = c('apt', 'install', sub('', 'r-cran-', install_from_cran, '-y --fix-missing')),
    stdout = TRUE,
    stderr = TRUE,
    wait = TRUE
  )

  tryCatch({
    pacman::p_load(char = libs_to_load, install = FALSE)

    # install and load rwRtools from GH (sans dependencies)
    pacman::p_load_current_gh("RWLab/rwRtools", dependencies = FALSE)

    # install and load rsims from GH (sans dependencies)
    if(load_rsims == TRUE)
      pacman::p_load_current_gh("Robot-Wealth/rsims", dependencies = FALSE)
  }, error = function(e) {
    print(e)
  })

  # output messages from install - optional, for debugging
  return(invisible(msg2))

}
