#' Create oauth app for use in the lab
#'
#' @return oauth app
#'
get_lab_app <- function() {
  # old oauth from gargle 0.5.0 (naughty naughty)
  # httr::oauth_app("rwlab", '603366585132-0l5tra7gl2i20iftqangp7iskte4f3s0.apps.googleusercontent.com', 'bQJYd0bRVkGJiWV-nSxK2Zgc', "urn:ietf:wg:oauth:2.0:oob")

  httr::oauth_app("rwlab", '598941630242-e973c0tf7k834upma9ld4m5n23fotet8.apps.googleusercontent.com', 'QPtBkrFmTKWpGmIWr-nU4eX2', "urn:ietf:wg:oauth:2.0:oob")
}
