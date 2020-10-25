#' Interactively authorise a Google Identity and load Oauth2 token
#'
#' Authorise a Google Identity to access The Lab's cloud infrastructure. The first call
#' in a session asks the user to interactively specify a Google Identity and provide the
#' resulting code back to the call site.
#'
#' Internally sets `gargle::gargle_options`:
#' - `gargle_oauth_email`
#' - `gargle_oath_cache`
#'
#' See \code{\link[gargle:gargle_options]{gargle_options}} for details.
#'
#'  TODO: How to set up a Google Identity if you don't have one and/or don't want to use a gmail account.
#'
#' @param oauth_email string, The email address you use to access The Lab
#' @param oauth_cache bool, Cache the OAuth token for reuse until expiry
#' @return an OAuth token object (invisibly)
#' @export
#' @examples
#' rwlab_gc_auth()
rwlab_gc_auth <- function(oauth_email, oauth_cache = TRUE) {
  options(
    rlang_interactive = TRUE,
    gargle_oauth_email = oauth_email,
    gargle_oauth_cache = oauth_cache
  )

  tt <- gargle::token_fetch(scopes = "https://www.googleapis.com/auth/cloud-platform")
  googleAuthR::gar_auth(token = tt)

}

rwlab_list_bucket_objects <- function(bucket = "rw_equity_research_sprint") {
  # TODO: if(.gcs_env$bucket != bucket) gcs_global_bucket(bucket)
  gcs_global_bucket(bucket)
  gcs_list_objects('rw_equity_research_sprint')
}
