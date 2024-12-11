# Functions for pulling data used in Trade Like a Quant

#' Load main asset classes daily  data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return Historical daily data of some of the main asset classes as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tlaq_get_historical_asset_class()
#' }
tlaq_get_historical_asset_class <- function(path = "tlaq", force_update = TRUE) {
  if(!dir.exists(path)) {
    dir.create(path)
  }

  if(!file.exists(file.path(path, glue::glue('main_asset_classes_daily_ohlc.csv'))) || force_update == TRUE) {
    download.file(
      url = "https://storage.googleapis.com/tlaq_public/main_asset_classes_daily_ohlc.csv",
      destfile = glue::glue("{path}/main_asset_classes_daily_ohlc.csv")
    )
  }

  df <- readr::read_csv(
    glue::glue("{path}/main_asset_classes_daily_ohlc.csv"),
    guess_max = 500000
  )

  # ensure date column is a date.
  df$date <- lubridate::date(df$date)

  df <- dplyr::arrange(df, date, ticker)

  df
}
