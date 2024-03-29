% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fx_data_utils.R
\name{fx_get_policy_rates}
\alias{fx_get_policy_rates}
\title{Load historical central bank policy rates}
\usage{
fx_get_policy_rates(currencies, path = "Policy-Rates", force_update = TRUE)
}
\arguments{
\item{currencies}{A chr vector of currencies}

\item{path}{The path to save the policy rate data}

\item{force_update}{Force download and overwrite exsiting files}
}
\value{
A long data.frame of historical CB rates
}
\description{
Load historical central bank policy rates
}
\examples{
\dontrun{
fx_get_policy_rates(c("AUD","BRL","CAD","CHF","CHN","DDK","EUR","GBP","HKD","JPY","MXN","NOK","NZD","PLN","RUB","SEK","SGD","TRY","USD","ZAR"))
}
}
