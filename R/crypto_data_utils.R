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
  df <- dplyr::arrange(df,ticker,date)

df
}

#' Load coincodex daily crypto data
#'
#' @param path The path to save the coincodex dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The coincodex dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_coincodex()
#' }
crypto_get_coincodex <- function(path = "coincodex", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('coincodex_marketcap.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("coincodex_marketcap.feather"),
      path = path
    )
  }

  df <- arrow::read_feather(glue::glue("{path}/coincodex_marketcap.feather")) %>%
    mutate(Date = lubridate::as_date(Date)) %>%
    arrange(Date, Ticker)

  df
}

#' Load Binance Spot 1d OHLCV data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The binance 1d spot OHLCV dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_binance_spot_1d()
#' }
crypto_get_binance_spot_1d <- function(path = "binance", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('binance_spot_production_1d_ohlc.csv'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("binance_spot_production_1d_ohlc.csv"),
      path = path
    )
  }

  df <- readr::read_csv(glue::glue("{path}/binance_spot_production_1d_ohlc.csv")) %>%
    mutate(date = lubridate::as_datetime(date)) %>%
    arrange(date, ticker)

  df
}

#' Load Binance Spot 1h OHLCV data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The binance 1h spot OHLCV dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_binance_spot_1h()
#' }
crypto_get_binance_spot_1h <- function(path = "binance", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('binance_spot_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("binance_spot_1h.feather"),
      path = path
    )
  }

  df <- arrow::read_feather(glue::glue("{path}/binance_spot_1h.feather")) %>%
    mutate(Datetime = lubridate::as_datetime(Datetime)) %>%
    arrange(Datetime, Ticker)

  df
}

#' Load Binance Perp 1h OHLCV data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The binance 1h perp OHLCV dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_binance_perps_1h()
#' }
crypto_get_binance_perps_1h <- function(path = "binance", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('binance_perps_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("binance_perps_1h.feather"),
      path = path
    )
  }

  df <- arrow::read_feather(glue::glue("{path}/binance_perps_1h.feather")) %>%
    mutate(Datetime = lubridate::as_datetime(Datetime)) %>%
    arrange(Datetime, Ticker)

  df
}

#' Load Binance Perp funding data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The binance perp funding dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_binance_perps_funding()
#' }
crypto_get_binance_perps_funding <- function(path = "binance", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('binance_perps_funding.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("binance_perps_funding.feather"),
      path = path
    )
  }

  df <- arrow::read_feather(glue::glue("{path}/binance_perps_funding.feather")) %>%
    rename("Funding_time_unix" = fundingTime, "funding_time" = fundingTimeHR, "funding_rate" = fundingRate) %>%
    select(Ticker, funding_time, funding_rate) %>%
    mutate(funding_time = lubridate::as_datetime(funding_time)) %>%
    arrange(funding_time, Ticker)

  df
}

#' Load Binance Coin-M Perp 1h OHLCV data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The binance 1h coin-M perp OHLCV dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_binance_coin_m_perps_1h()
#' }
crypto_get_binance_coin_m_perps_1h <- function(path = "binance", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('binance_perps_coin_m_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("binance_perps_coin_m_1h.feather"),
      path = path
    )
  }

  df <- arrow::read_feather(glue::glue("{path}/binance_perps_coin_m_1h.feather")) %>%
    mutate(Datetime = lubridate::as_datetime(Datetime)) %>%
    arrange(Datetime, Ticker)

  df
}

#' Load Binance Coin-M Perp funding data
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The binance coin-M perp funding dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_binance_coin_m_perps_funding()
#' }
crypto_get_binance_coin_m_perps_funding <- function(path = "binance", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('binance_perps_coin_m_funding.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("binance_perps_coin_m_funding.feather"),
      path = path
    )
  }

  df <- arrow::read_feather(glue::glue("{path}/binance_perps_coin_m_funding.feather")) %>%
    rename("Funding_time_unix" = fundingTime, "funding_time" = fundingTimeHR, "funding_rate" = fundingRate) %>%
    select(Ticker, funding_time, funding_rate) %>%
    mutate(funding_time = lubridate::as_datetime(funding_time)) %>%
    arrange(funding_time, Ticker)

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

  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%d %H:%M:%S",tz="UTC")

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

  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

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


  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz="UTC")


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


  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%d %H:%M:%S",tz="UTC")


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


  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  df
}


#' Load cleaned spot hourly data from FTX
#' This dataset is cleaned from tokenzied stocks, leveraged tokens and FX pairs (only includes USDC denominated pairs)
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The spot dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_clean_spot()
#' }
crypto_get_clean_spot <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_clean_spot_ohlc_1h.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_clean_spot_ohlc_1h.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_clean_spot_ohlc_1h.feather'))
  )


  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  df
}


#' Load rebalance token data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The rebalance trades dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_rebalance_trades()
#' }
crypto_get_rebalance_trades <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_token_rebalance_trades.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_token_rebalance_trades.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_token_rebalance_trades.feather'))
  )


  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%d %H:%M:%S",tz="UTC")

  df
}


#' Load Expired futures hourly data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The expired futures dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_expired_futures()
#' }
crypto_get_expired_futures <- function(path = "ftx", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('ftx_expired_futures_1h_ohlc.feather'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_expired_futures_1h_ohlc.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_expired_futures_1h_ohlc.feather'))
  )

  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  df
}


#' Load One minnute perpetuals data from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The one minute perpetuals dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_minute_perpetuals()
#' }
crypto_get_minute_perpetuals <- function(path = "ftx", force_update = TRUE) {

  if(!file.exists(file.path(path, glue::glue('ftx_perps_1m_ohlc.feather'))) || force_update == TRUE) {
    print("Please Wait. This is a large dataset.")
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_perps_1m_ohlc.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_perps_1m_ohlc.feather'))
  )

  #Convert chr to datetime
  df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz="UTC")

  df
}

#' Load top-of-book sample for BTC-PERP from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The top-of-book snapshot dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_top_of_book_sample()
#' }
crypto_get_top_of_book_sample <- function(path = "ftx", force_update = TRUE) {

  if(!file.exists(file.path(path, glue::glue('ftx_btc_perp_bbo_sample.feather'))) || force_update == TRUE) {
    print("Please Wait. This is a large dataset.")
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_btc_perp_bbo_sample.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_btc_perp_bbo_sample.feather'))
  )

  df
}

#' Load trades sample for BTC-PERP from FTX
#'
#' @param path The path to save the dataset locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The trades snapshot dataset as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- crypto_get_trades_sample()
#' }
crypto_get_trades_sample <- function(path = "ftx", force_update = TRUE) {

  if(!file.exists(file.path(path, glue::glue('ftx_btc_perp_trades_sample.feather'))) || force_update == TRUE) {
    print("Please Wait. This is a large dataset.")
    transfer_lab_object(
      pod = "Crypto",
      object = glue::glue("ftx_btc_perp_trades_sample.feather"),
      path = path
    )
  }

  df <- feather::read_feather(
    file.path(path, glue::glue('ftx_btc_perp_trades_sample.feather'))
  )

  df
}

