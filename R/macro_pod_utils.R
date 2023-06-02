# Macro Research Pod data utilities

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
  df <- dplyr::arrange(df, date, ticker)

df
}

#' Load expiring futures daily OHLC
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The futures dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_expiring_futures()
#' }
macro_get_expiring_futures <- function(path = "macropod", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('futures_1d_ohlc.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = glue::glue("futures_1d_ohlc.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('futures_1d_ohlc.feather'))
  )

  df
}

#' Load RP expiring futures daily OHLC
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The futures dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_expiring_futures()
#' }
macro_get_expiring_rp_futures <- function(path = "macropod", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('rp_futures_1d_ohlc.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = glue::glue("rp_futures_1d_ohlc.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('rp_futures_1d_ohlc.feather'))
  )

  df
}

#' Load expiring VX futures daily OHLC plus metadata
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The VX futures dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_expiring_vx_futures()
#' }
macro_get_expiring_vx_futures <- function(path = "macropod", force_update = TRUE) {
  obj <- "vx_futures_1d_ohlc.feather"
  if(!file.exists(file.path(path, obj)) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = obj,
      path = path
    )
  }

  df <- arrow::read_feather(
    file.path(path, obj)
  ) %>%
    mutate(
      date = lubridate::as_date(date),
      expiry = lubridate::as_date(expiry)
    )

  df
}

#' Load daily rates data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The rates dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_rates()
#' }
macro_get_rates <- function(path = "macropod", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('tbill_rates.csv'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = glue::glue("tbill_rates.csv"),
      path = path
    )
  }

  df <- readr::read_csv(
    file.path(path, glue::glue('tbill_rates.csv'))
  )

  df
}


#' Load government bond returns dataset
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite existing files
#'
#' @return The govt bonds returns dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_govt_bond_returns()
#' }
macro_get_govt_bond_returns <- function(path = "macropod", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('bond_returns.csv'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = glue::glue("bond_returns.csv"),
      path = path
    )
  }

  df <- readr::read_csv(
    file.path(path, glue::glue('bond_returns.csv'))
  )

  df
}




#' Load VIX and VIX3M index daily  data
#'
#' @param path The path to save the vix/vix3m dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return Historical daily index data of VIX and VIX3M as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- macro_get_vix_vix3m()
#' }
macro_get_vix_vix3m <- function(path = "macropod", force_update = TRUE) {
  filename <- 'vix_vix3m.csv'
  if(!file.exists(file.path(path, filename)) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Macro",
      object = filename,
      path = path
    )
  }

  df <- readr::read_csv(
    file.path(path, filename),
    guess_max = 500000
  )
  df <- dplyr::arrange(df, date, ticker)

  df
}
