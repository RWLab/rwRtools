# Equity Factors data utilities

#' Load stat arb spreads data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite existing files
#'
#' @return The stat arb spreads dataset as a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- equity_get_statarb_spreads()
#' }
equity_get_statarb_spreads <- function(path = "equityfactors", force_update = TRUE) {
  if(!file.exists(file.path(path, "spreads.feather")) || force_update == TRUE) {
    transfer_lab_object(
      pod = "EquityFactors",
      object = "statarb/spreads.feather",
      path = path
    )
  }

  arrow::read_feather(file.path(path, "spreads.feather"))
}

#' Load stat arb prices data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite existing files
#'
#' @return The stat arb prices dataset as a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- equity_get_statarb_prices()
#' }
equity_get_statarb_prices <- function(path = "equityfactors", force_update = TRUE) {
  if(!file.exists(file.path(path, "prices.feather")) || force_update == TRUE) {
    transfer_lab_object(
      pod = "EquityFactors",
      object = "statarb/prices.feather",
      path = path
    )
  }

  arrow::read_feather(file.path(path, "prices.feather"))
}

#' Load stat arb acquisitions data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite existing files
#'
#' @return The stat arb acquisitions dataset as a data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- equity_get_statarb_acquisitions()
#' }
equity_get_statarb_acquisitions <- function(path = "equityfactors", force_update = TRUE) {
  if(!file.exists(file.path(path, "acquisitions.feather")) || force_update == TRUE) {
    transfer_lab_object(
      pod = "EquityFactors",
      object = "statarb/acquisitions.feather",
      path = path
    )
  }

  arrow::read_feather(file.path(path, "acquisitions.feather"))
}
