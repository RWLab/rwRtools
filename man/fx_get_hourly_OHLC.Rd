% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fx_data_utils.R
\name{fx_get_hourly_OHLC}
\alias{fx_get_hourly_OHLC}
\title{Load hourly market data for multiple tickers}
\usage{
fx_get_hourly_OHLC(tickers, path = "Hourly", force_update = TRUE)
}
\arguments{
\item{tickers}{A chr vector of the tickers to load.}

\item{path}{The path to the hourly resolution price data.}

\item{force_update}{Force download and overwrite exsiting files}
}
\value{
A long data.frame of market data.
}
\description{
Returns a data.frame of market data with columns
Ticker, Date, Time, Open, High, Low, Close, Volume
}
\examples{
\dontrun{
fx_get_hourly_OHLC(c("EURUSD", "AUDUSD"))
}
}
