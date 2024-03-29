% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macro_pod_utils.R
\name{macro_get_earnings}
\alias{macro_get_earnings}
\title{Load earnings data}
\usage{
macro_get_earnings(path = "macropod", force_update = TRUE)
}
\arguments{
\item{path}{The path to save the dataset locally.}

\item{force_update}{Force download and overwrite exsiting files}
}
\value{
The earnings dataset as a tibble.
}
\description{
Load earnings data
}
\examples{
\dontrun{
df <- macro_get_earnings()
}
}
