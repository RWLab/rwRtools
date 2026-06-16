# FX Data Utilities

#' Load a Zorro Assets List
#'
#' If the Assets List file does not exist in `path`, download it from
#' the Research Pod's GCS bucket.
#'
#' @param asset_list The name of the Zorro Asset List to load, without the
#' file extension.
#' @param path The path to save the Asset List locally.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return The Asset List as a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' fx_get_asset_list("AssetsDWX-FX-USD")
#' }
fx_get_asset_list <- function(asset_list, path = "Zorro-Assets-Lists", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue('{asset_list}.csv'))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "FX",
      object = glue::glue("feather/Zorro-Assets-Lists/{asset_list}.csv"),
      path = path
    )
  }

  readr::read_csv(
    file.path(path, glue::glue('{asset_list}.csv')),
    col_types = 'cddddddddddc'
  )
}

#' Load historical daily price data of a ticker
#'
#' Returns a data.frame of market data with columns
#' Ticker, Date, Open, High, Low, Close, Volume
#'
#' @param ticker The ticker to load.
#' @param path The path to save the daily resolution price data.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return A data.frame of market data.
#'
#' @examples
#' \dontrun{
#' fx_get_daily_OHLC_ticker("EURUSD")
#' }
fx_get_daily_OHLC_ticker <- function(ticker, path = "Daily", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue("{ticker}.feather"))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "FX",
      object = glue::glue("feather/Daily/{ticker}.feather"),
      path = path
    )
  }
  try(
    td <- feather::read_feather(glue::glue("{path}/{ticker}.feather"))
  )
  colnames(td) <- c('Date', 'Open' ,'High', 'Low', 'Close', 'Volume', 'Ticker')
  td$Date <- lubridate::as_date(as.character(td$Date))
  td$Ticker <- as.character(td$Ticker)
  td
}

#' Load historical hourly price data of a ticker
#'
#' Returns a data.frame of market data with columns
#' Ticker, Date, Time, Open, High, Low, Close, Volume
#'
#' @param ticker The ticker to load.
#' @param path The path to save the hourly resolution price data.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return A data.frame of market data.
#'
#' @examples
#' \dontrun{
#' fx_get_daily_OHLC_ticker("EURUSD")
#' }
fx_get_hourly_OHLC_ticker <- function(ticker, path = "Hourly", force_update = TRUE) {
  if(!file.exists(file.path(path, glue::glue("{ticker}.feather"))) || force_update == TRUE) {
    transfer_lab_object(
      pod = "FX",
      object = glue::glue("feather/Hourly/{ticker}.feather"),
      path = path
    )
  }
  td <- feather::read_feather(glue::glue("{path}/{ticker}.feather"))
  colnames(td) <- c('Datetime', 'Open' ,'High', 'Low', 'Close', 'Volume', 'Ticker')
  td$Datetime <- lubridate::as_datetime(as.character(td$Datetime))
  td$Ticker <- as.character(td$Ticker)
  return(td)
}

#' Load daily market data for multiple tickers
#'
#' Returns a data.frame of market data with columns
#' Ticker, Date, Open, High, Low, Close, Volume
#'
#' @param tickers A chr vector of the tickers to load.
#' @param path The path to the daily resolution price data.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return A long data.frame of market data.
#' @export
#'
#' @examples
#' \dontrun{
#' fx_get_daily_OHLC(c("EURUSD", "AUDUSD"))
#' }
fx_get_daily_OHLC <- function(tickers, path = "Daily", force_update = TRUE) {
  tickers <- gsub('/', '', tickers) # filenames do not have /'s but Zorro assetlists do
  l <- purrr::map(tickers, fx_get_daily_OHLC_ticker, path = path, force_update = force_update)
  dplyr::bind_rows(l)
}

#' Load hourly market data for multiple tickers
#'
#' Returns a data.frame of market data with columns
#' Ticker, Date, Time, Open, High, Low, Close, Volume
#'
#' @param tickers A chr vector of the tickers to load.
#' @param path The path to the hourly resolution price data.
#' @param force_update Force download and overwrite exsiting files
#'
#' @return A long data.frame of market data.
#' @export
#'
#' @examples
#' \dontrun{
#' fx_get_hourly_OHLC(c("EURUSD", "AUDUSD"))
#' }
fx_get_hourly_OHLC <- function(tickers, path = "Hourly", force_update = TRUE) {
  tickers <- gsub('/', '', tickers) # filenames do not have /'s but Zorro assetlists do
  l <- purrr::map(tickers, fx_get_hourly_OHLC_ticker, path = path, force_update = force_update)
  dplyr::bind_rows(l)
}

#' Convert price data to a common quote currency.
#'
#' Convert exchange rates containing `quote_currency` such that
#' `quote_currency` is the quote currency of the pair.
#' Any ticker in `prices_df` that is not FX is discarded.
#' Any ticker that does not contain `quote_currency` is discarded.
#'
#' @param prices_df A data.frame containing tickers and prices.
#' @param quote_currency The desired common quote_currency.
#'
#' @return A data.frame of tickers and prices with a common quote currency.
#' @export
#'
#' @examples
#' \dontrun{
#' fx_convert_common_quote_currency(prices, "USD")
#' }
fx_convert_common_quote_currency <- function(prices_df, quote_currency = "USD") {
  # convert records where the base currency is the desired quote currency
  prices_df %>%
    dplyr::mutate(Base = stringr::str_sub(Ticker, 1, 3)) %>%
    dplyr::mutate(Quote = stringr::str_sub(Ticker, -3)) %>%
    dplyr::filter(Base == quote_currency & Quote != quote_currency) %>%
    dplyr::mutate(
      Ticker = paste0(Quote,Base),
      Open = 1/Open,
      NewHigh = 1/Low,
      Low = 1/High,
      High = NewHigh,
      Close = 1/Close
    ) %>%
    dplyr::select(c(-Base, -Quote, -NewHigh)) %>%
    # append records where the Quote currency is the desired quote currency
    dplyr::bind_rows(
      prices_df %>%
        dplyr::mutate(Base = stringr::str_sub(Ticker, 1, 3)) %>%
        dplyr::mutate(Quote = stringr::str_sub(Ticker, -3)) %>%
        dplyr::filter(Base != quote_currency & Quote == quote_currency) %>%
        dplyr::select(c(-Base, -Quote))
    )
}

#' Load historical central bank policy rates
#'
#' @param currencies A chr vector of currencies
#' @param path The path to save the policy rate data
#' @param force_update Force download and overwrite exsiting files
#'
#' @return A long data.frame of historical CB rates
#' @export
#'
#' @examples
#' \dontrun{
#' fx_get_policy_rates(c("AUD","BRL","CAD","CHF","CHN","DDK","EUR","GBP","HKD","JPY","MXN","NOK","NZD","PLN","RUB","SEK","SGD","TRY","USD","ZAR"))
#' }
fx_get_policy_rates <- function(currencies, path = "Policy-Rates", force_update = TRUE) {
  l <- purrr::map(currencies, function(currency) {
    if(!file.exists(file.path(path, glue::glue("{currency}.csv"))) || force_update == TRUE) {
      transfer_lab_object(
        pod = "FX",
        object = glue::glue("feather/Policy-Rates/{currency}.csv"),
        path = path
      )
    }
    td <- data.frame(matrix(ncol=3, nrow=0))
    try(td <- cbind(
      Currency = currency,
      readr::read_csv(
        file.path(path, glue::glue("{currency}.csv")),
        col_types = 'Dd'
      )))
    colnames(td) <- c('Currency', 'Date', 'Rate')
    td$Date <- lubridate::as_date(as.character(td$Date))
    td$Currency <- as.character(td$Currency)
    td %>%
      na.omit() %>%
      dplyr::arrange(Date)
  })
  dplyr::bind_rows(l)
}

#' Calculate currency total return indexes
#'
#' Records without corresponding rate data are removed from returned
#' data.frame.
#'
#' @param prices_df A data.frame of historical spot prices
#' @param policy_rates_df A data.frame of historical central bank policy rates
#'
#' @return A data.frame of index returns: spot, interest and total
#' @export
fx_total_return_index <- function(prices_df, policy_rates_df) {

  # Keep policy-rate input clean to avoid accidental extra columns after joins
  policy_rates_clean <- policy_rates_df %>%
    dplyr::select(Currency, Date, Rate)

  # Build a policy-rate panel over all FX price dates and policy-rate dates.
  # This lets us carry policy rates forward before joining to FX prices.
  all_dates <- dplyr::bind_rows(
    prices_df %>% dplyr::distinct(Date),
    policy_rates_clean %>% dplyr::distinct(Date)
  ) %>%
    dplyr::distinct(Date)

  currencies <- policy_rates_clean %>%
    dplyr::distinct(Currency)

  policy_panel <- tidyr::expand_grid(
    Date = all_dates$Date,
    Currency = currencies$Currency
  ) %>%
    dplyr::left_join(policy_rates_clean, by = c("Currency", "Date")) %>%
    dplyr::arrange(Currency, Date) %>%
    dplyr::group_by(Currency) %>%
    tidyr::fill(Rate, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Rate = Rate * 0.01)

  extended_prices <- prices_df %>%
    # split ticker into Base and Quote currencies
    dplyr::mutate(
      Base = stringr::str_sub(Ticker, 1, 3),
      Quote = stringr::str_sub(Ticker, -3)
    ) %>%
    dplyr::left_join(
      policy_panel %>% dplyr::rename(Base_Rate = Rate),
      by = c("Base" = "Currency", "Date" = "Date")
    ) %>%
    dplyr::left_join(
      policy_panel %>% dplyr::rename(Quote_Rate = Rate),
      by = c("Quote" = "Currency", "Date" = "Date")
    ) %>%
    dplyr::group_by(Ticker) %>%
    dplyr::arrange(Ticker, Date) %>%
    dplyr::mutate(
      Rate_Diff = Base_Rate - Quote_Rate,

      Daycount_Fraction =
        (as.numeric(Date) - as.numeric(dplyr::lag(Date))) / 365,

      # Spot return from t-1 to t
      Spot_Returns = Close / dplyr::lag(Close) - 1,

      # Interest return earned over the period from t-1 to t.
      # Use lagged rates because those are the rates known/accruing during the period.
      Interest_Returns =
        (
          (1 + dplyr::lag(Base_Rate) * Daycount_Fraction) /
            (1 + dplyr::lag(Quote_Rate) * Daycount_Fraction)
        ) - 1,

      # Kept for backwards compatibility.
      # Approximate quote-currency interest accrual on one unit of base exposure,
      # using previous spot value as the period's starting capital.
      Interest_Accrual_on_Spot =
        Interest_Returns * dplyr::lag(Close)
    ) %>%
    # Drop only rows needed for return/index construction.
    # Avoid broad na.omit because prices_df may contain unrelated nullable columns.
    tidyr::drop_na(
      Base_Rate,
      Quote_Rate,
      Daycount_Fraction,
      Spot_Returns,
      Interest_Returns,
      Interest_Accrual_on_Spot
    )

  # calculate total return indexes
  extended_prices %>%
    dplyr::group_by(Ticker) %>%
    dplyr::arrange(Ticker, Date) %>%
    dplyr::mutate(
      Spot_Return_Index = cumprod(1 + Spot_Returns),

      # Same column name as original
      Interest_Return_Index = cumprod(1 + Interest_Returns),

      # Exact multiplicative combination of spot and carry
      Total_Return_Index = cumprod(
        (1 + Spot_Returns) * (1 + Interest_Returns)
      )
    ) %>%
    dplyr::ungroup()
}

#' Unique currencies from fx pairs
#'
#' Get unique currencies from a data frame of price data.
#'
#' @param prices_df A data.frame of fx price data
#'
#' @return A chr vector of unique currencies
#' @export
#'
#' @examples
#' \dontrun{
#' fx_get_unique_currencies(prices_df)
#' }
fx_get_unique_currencies <- function(prices_df) {
  base_tickers <- prices_df %>%
    dplyr::mutate(ccy = stringr::str_sub(Ticker, 1, 3)) %>%
    dplyr::distinct(ccy)

  quote_tickers <- prices_df %>%
    dplyr::mutate(ccy = stringr::str_sub(Ticker, -3)) %>%
    dplyr::distinct(ccy)

  as.vector(unique(c(base_tickers$ccy, quote_tickers$ccy)))
}

