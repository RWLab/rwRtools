% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fx_data_utils.R
\name{fx_convert_common_quote_currency}
\alias{fx_convert_common_quote_currency}
\title{Convert price data to a common quote currency.}
\usage{
fx_convert_common_quote_currency(prices_df, quote_currency = "USD")
}
\arguments{
\item{prices_df}{A data.frame containing tickers and prices.}

\item{quote_currency}{The desired common quote_currency.}
}
\value{
A data.frame of tickers and prices with a common quote currency.
}
\description{
Convert exchange rates containing \code{quote_currency} such that
\code{quote_currency} is the quote currency of the pair.
Any ticker in \code{prices_df} that is not FX is discarded.
Any ticker that does not contain \code{quote_currency} is discarded.
}
\examples{
\dontrun{
fx_convert_common_quote_currency(prices, "USD")
}
}
