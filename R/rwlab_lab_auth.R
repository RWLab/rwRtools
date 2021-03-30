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
#'  TODO: How to set up a Google Identity if you don't have one and/or don't want to use a gmail account.
#'
#' @param oauth_email string, The email address you use to access The Lab. If NA (default), select interactively.
#' @param oauth_cache bool, Cache the OAuth token for reuse until expiry
#' @return an OAuth token object (invisibly)
#'
#' @import httr
#' @export
#' @examples
#' \dontrun{rwlab_data_auth()}
rwlab_data_auth <- function(oauth_email = NA, oauth_cache = FALSE) {

  options(
    gargle_oauth_email = oauth_email,
    gargle_oauth_cache = oauth_cache,
    gargle_oob_default = TRUE
  )

  if (length(Sys.glob("/usr/local/lib/python*/dist-packages/google/colab/_ipython.py")) > 0 || interactive()) {
    R.utils::reassignInPackage("is_interactive", pkgName = "httr", function() return(TRUE))
    options(
      rlang_interactive = TRUE
    )
  }

  googleAuthR::gar_auth_configure(app = get_lab_app())

  # this wraps gargle::token_fetch, which steps through several credential-fetching functions.
  googleAuthR::gar_auth(
    scopes = "https://www.googleapis.com/auth/cloud-platform",
    use_oob = gargle::gargle_oob_default(),
    app = googleAuthR::gar_oauth_app()
  )

}

