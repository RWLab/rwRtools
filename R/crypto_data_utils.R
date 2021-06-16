# Crypto data utilities

#' Load coinmetrics daily crypto data
#'
#' @param path The path to save the Coinmetrics dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The Coinmetrics dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_coinmetrics()
#' }
crypto_get_coinmetrics <- function(path = "coinmetrics", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('coinmetrics.csv'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("coinmetrics.csv"),
      path = path
    )
  }

 df <- readr::read_csv(
    file.path(path, glue::glue('coinmetrics.csv')),
    guess_max = 500000


  )
  df <- dplyr::arrange(df,asset, date)
  df <- dplyr::select(df,asset,date,PriceUSD,PriceBTC,Price_in_USD_or_index_value)

df
}


#' Load Coin lending rates hourly data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The Coin lending rates dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_lending_rates()
#' }
crypto_get_lending_rates <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_coin_lending_rates.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_coin_lending_rates.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_coin_lending_rates.feather'))
    )

  #Rename column
  df <- dplyr::rename(df,datetime = time)

  #Convert chr to datetime
  df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

  df
}


#' Load futures hourly data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The futures dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_futures()
#' }
crypto_get_futures <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_futures_ohlc_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_futures_ohlc_1h.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_futures_ohlc_1h.feather'))
  )

  #Rename column
  df <- dplyr::rename(df,datetime = startTime)

  #Convert chr to datetime
  df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  df <- dplyr::select(df, ticker, datetime, open, high, low, close, volume)

  df
}


#' Load Index hourly data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The index dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_index()
#' }
crypto_get_index <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_index_ohlc_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_index_ohlc_1h.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_index_ohlc_1h.feather'))
  )

  #Rename column
  df <- dplyr::rename(df,datetime = startTime)

  #Convert chr to datetime
  df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  #Volume is NaN for index data
  df <- dplyr::select(df, ticker, datetime, open, high, low, close)

  df
}


#' Load Perpetual Funding Rates data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The Perpetual Funding Rates dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_perp_rates()
#' }
crypto_get_perp_rates <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_perpetual_funding_rates.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_perpetual_funding_rates.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_perpetual_funding_rates.feather'))
  )

  #Rename column
  df <- dplyr::rename(df,datetime = time)

  #Convert chr to datetime
  df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC")


  df
}


#' Load spot hourly data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The spot dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_spot()
#' }
crypto_get_spot <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_spot_ohlc_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_spot_ohlc_1h.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_spot_ohlc_1h.feather'))
  )

  #Rename column
  df <- dplyr::rename(df,datetime = startTime)

  #Convert chr to datetime
  df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  df <- dplyr::select(df, ticker, datetime, open, high, low, close, volume)

  df
}
