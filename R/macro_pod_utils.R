# Macro Research Pod data utilities

#' Load main asset classes daily  data
#'
#' @param path The path to save the Coinmetrics dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return Historical daily data of some of the main asset classes as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_historical_asset_class()
#' }
macro_get_historical_asset_class <- function(path = "macropod", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('main_asset_classes_daily_ohlc.csv'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = glue::glue("main_asset_classes_daily_ohlc.csv"),
      path = path
    )
  }

 df <- readr::read_csv(
    file.path(path, glue::glue('main_asset_classes_daily_ohlc.csv')),
    guess_max = 500000


  )
  df <- dplyr::arrange(df,ticker,date)

df
}