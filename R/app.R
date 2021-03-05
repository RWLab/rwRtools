#' Create oauth app for use in the lab
#'
#' Surprisingly, the “client secret” in an oauth_app() is not a secret. It’s not equivalent to a password, and if you are writing an API wrapper package, it should be included in the package. [(If you don’t believe me, here are google’s comments on the topic.)](https://developers.google.com/identity/protocols/OAuth2#installed)
#'
#' @return oauth app
#'
get_lab_app <- function() {
  # old oauth from gargle 0.5.0 (naughty naughty)
  httr::oauth_app("rwlab", '603366585132-0l5tra7gl2i20iftqangp7iskte4f3s0.apps.googleusercontent.com', 'bQJYd0bRVkGJiWV-nSxK2Zgc', "urn:ietf:wg:oauth:2.0:oob")

  # new rw oauth...pending verification
  # httr::oauth_app("rwlab", '598941630242-e973c0tf7k834upma9ld4m5n23fotet8.apps.googleusercontent.com', 'QPtBkrFmTKWpGmIWr-nU4eX2', "urn:ietf:wg:oauth:2.0:oob")
}
