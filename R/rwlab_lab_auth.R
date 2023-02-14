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
# rwlab_data_auth <- function(oauth_email = NA, oauth_cache = FALSE) {
# # old version: need our own oauth app going forward
#
#   options(
#     gargle_oauth_email = oauth_email,
#     gargle_oauth_cache = oauth_cache,
#     gargle_oob_default = TRUE
#   )
#
#   if (length(Sys.glob("/usr/local/lib/python*/dist-packages/google/colab/_ipython.py")) > 0 || interactive()) {
#     R.utils::reassignInPackage("is_interactive", pkgName = "httr", function() return(TRUE))
#     options(
#       rlang_interactive = TRUE
#     )
#   }
#
#
  # googleAuthR::gar_auth_configure(app = get_lab_app())
  #
  # # this wraps gargle::token_fetch, which steps through several credential-fetching functions.
  # googleAuthR::gar_auth(
  #   scopes = "https://www.googleapis.com/auth/cloud-platform",
  #   use_oob = gargle::gargle_oob_default(),
  #   app = googleAuthR::gar_oauth_app()
  # )
# }
rwlab_data_auth <- function (json = json_string, token = NULL, email = NULL)
{
  # can no longer piggy back off tidyverse's OAuth app.
  # will need to generate our own
  # this will do in the interim.
  googleCloudStorageR:::set_scopes()
  if (is.null(json)) {
    gar_auth(token = token, email = email, package = "googleCloudStorageR")
  }
  else {
    # gar_auth_service(json_file = json_file)
    scope = getOption("googleAuthR.scopes.selected")
    scope <- paste(scope, collapse = " ")
    info <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    if (!identical(info[["type"]], "service_account")) {
      gargle_debug(c(
        "JSON does not appear to represent a service account",
        "Did you provide the JSON for an OAuth client instead of for a \\
       service account?"
      ))
      return()
    }
    # I add email scope explicitly, whereas I don't need to do so in
    # credentials_user_oauth2(), because it's done in Gargle2.0$new().
    scopes <- gargle:::normalize_scopes(gargle:::add_email_scope(scope))
    token <- httr::oauth_service_token(
      ## FIXME: not sure endpoint is truly necessary, but httr thinks it is.
      ## https://github.com/r-lib/httr/issues/576
      endpoint = gargle:::gargle_oauth_endpoint(),
      secrets = info,
      scope = scopes,
      sub = NULL  #subject
    )
    if (is.null(token$credentials$access_token) ||
        !nzchar(token$credentials$access_token)) {
      return(NULL)
    } else {
      gargle:::gargle_debug("service account email: {.email {token_email(token)}}")
    }


    if (!googleAuthR:::is.tokenservice(token)) {
      stop("Invalid TokenServiceAccount", call. = FALSE)
    }
    googleAuthR:::.auth$set_cred(token)
    googleAuthR:::.auth$set_auth_active(TRUE)
    cat("Authorization successful!")
    invisible(token)
  }
}

