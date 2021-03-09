#' Create oauth app for use in the lab
#'
#' Surprisingly, the “client secret” in an oauth_app() is not a secret. It’s not equivalent to a password, and if you are writing an API wrapper package, it should be included in the package. [(If you don’t believe me, here are google’s comments on the topic.)](https://developers.google.com/identity/protocols/OAuth2#installed)
#'
#' @return oauth app
#'
get_lab_app <- function() {
  gla()
}

# snippet: rw load fx research pod libraries for data utils v0.1a
# if(!require("pacman")) install.packages("pacman")
# pacman::p_load_current_gh("RWLab/rwRtools", dependencies = TRUE)
# pacman::p_load(tidyverse)

# # we need to revert to an earlier version of gragle, which also requires unloading googleAuthR
# unloadNamespace("googleAuthR")
# unloadNamespace("gargle")
# package = "https://cran.r-project.org/package=gargle&version=0.5.0"
# utils::install.packages(pkgs = package, repos = NULL)
# library(gargle)
