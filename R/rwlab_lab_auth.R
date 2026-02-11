#' Interactively authorise a Google Identity and load Oauth2 token
#'
#' Authorise a Google Identity to access The Lab's cloud infrastructure via web browser. The first call
#' in a session asks the user to interactively specify a Google Identity and provide the
#' resulting code back to the call site.
#'
#' Internally sets the following options:
#' - `gargle::gargle_oauth_email`
#' - `gargle::gargle_oath_cache`
#' - `gargle::gargle_oob_default`
#' - `rlang::rlang_interactive` (if called from an interactive session)
#'
#' See \code{\link[gargle:gargle_options]{gargle_options}} for details.
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{rwlab_data_auth()}
rwlab_data_auth <- function ()
{
  # use the preconfigured OAuth client from gargle
  # this will break at some point and we will need our own OAuth client
  # but should continue to work in colab
  if(!gargle:::is_google_colab()) {
    options(gargle_oauth_client = gargle:::goc_web())
  }

  # force interactive session
  options(gargle_oauth_email = NULL)  # do not preselect an email
  options(gargle_oob_default = TRUE) # use out-of-band (OOB) authentication
  options(rlang_interactive = TRUE)

  # interactive authentication
  token <- gargle::token_fetch(scopes = "https://www.googleapis.com/auth/cloud-platform")

  # use token with googleCloudStorageR
  googleCloudStorageR::gcs_auth(token = token)
}

