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
directly from apt, which is much faster than compiling
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
"
load_libraries <- function(load_rsims = TRUE, extra_libraries = c()) {

  bash_script <- '#!/bin/bash

  ## Setup minimal environment for bspm
  apt update -qq && apt install --yes --no-install-recommends \\
    python3-dbus python3-gi python3-apt \\
    make \\
    wget \\
    ca-certificates \\
    gnupg

  ## Add keys for r2u
  wget -qO- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | \\
      tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc

  ## Add r2u repository
  echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu jammy main" \\
      > /etc/apt/sources.list.d/cranapt.list

  ## Update apt
  apt update -qq

  ## Install bspm through apt to avoid conflicts
  apt install --yes --no-install-recommends r-cran-bspm

  ## Enable bspm globally in R
  RHOME=$(R RHOME)
  echo "suppressMessages(bspm::enable())" >> ${RHOME}/etc/Rprofile.site
  echo "options(bspm.version.check=FALSE)" >> ${RHOME}/etc/Rprofile.site
  '

  # write bash script to a file
  writeLines(bash_script, "setup_bspm.sh")

  Sys.chmod("setup_bspm.sh", mode = "0755")
  system("sudo ./setup_bspm.sh")

  readRenviron("/etc/R/Renviron")
  source("/etc/R/Rprofile.site")

  options(Ncpus = parallel::detectCores())

  if (!require(pacman)) install.packages("pacman")
  library(pacman)

  core_packages <- c(
    'googleCloudStorageR', 'googleAuthR', 'assertthat', 'R.oo', 'R.utils', 
    'foreach', 'doParallel', 'xts', 'Rcpp', 'TTR', 'arrow', 'feather', 'gtable'
  )

  # bspm handles these installations via apt automatically
  p_load(char = core_packages, install = TRUE, update = FALSE, character.only = TRUE)

  p_load_gh("RWLab/rwRtools", dependencies = FALSE, update = FALSE)

  if (length(extra_libraries) > 0) {
    p_load(char = extra_libraries, install = TRUE, update = FALSE, character.only = TRUE)
  }

  if (load_rsims) {
    rsims_dependencies <- c('roll', 'here')
    p_load(char = rsims_dependencies, install = TRUE, update = FALSE, character.only = TRUE)
    p_load_gh("Robot-Wealth/rsims", dependencies = FALSE, update = FALSE)
  }

  p_load(tidyverse, install = TRUE, update = FALSE)

  message("All packages successfully loaded using bspm.")
}