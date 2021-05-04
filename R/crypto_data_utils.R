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
    col_types = 'Dddddddddddddddddddddddddddddddddddddddddddddddc'


  )
  df <- dplyr::arrange(df,asset, date)

df
}
