#' Get Research Pod metadata
#'
#' Returns bucket and dataset names for a specified pod,
#' or all pods if `pod` is unspecified.
#'
#' @param pod string, The name of the Research Pod. If NA, data for all pods is returned.
#'
#' @return List of metadata for the specified Research Pod, or list of lists of metadata for all Pods if `pod` is NA.
#' @export
#'
#' @examples
#' bucket = get_pod_meta("EquityFactors")[["bucket"]]
#' datasets = get_pod_meta("EquityFactors")[["datasets"]]
#' all_pods_meta = get_pod_meta()
get_pod_meta <- function(pod = NA) {
  # intent is to make explicit the data to be used in a Research Pod
  # also if we change the data or GCS bucket, we only need to make changes here
  pod_meta <- list(
    EquityFactors = list(
      bucket = "equity_factors_research_pod",
      datasets = c("R1000_ohlc_1d.feather", "R1000_fundamentals_1d.feather"),
      essentials = c("R1000_ohlc_1d.feather"),
      prices = c("R1000_ohlc_1d.feather")
      ),
    FX = list(
      bucket = "fx_research_pod",
      datasets = NA,
      essentials = NA,
      prices = NA
    )
  )

  if(is.na(pod)) {
    pod_meta
  }  else {
    if(is.null(pod_meta[[pod]]))
      stop("pod not found in list of Research Pods.")
    pod_meta[[pod]]
  }

}

#' Get the names of The Lab's Research Pods
#'
#' @return A character vector of the names of The Lab's Research Pods
#' @export
#'
#' @examples
#' list_pods()
list_pods <- function() {

  names(get_pod_meta())

}

#' Transfer all Research Pod data from Google Cloud to disk
#'
#' Requires authorisation. Set up prior with `rwlab_gc_auth`.
#'
#' @param pod string, The name of the research pod. Options: "Equity Factors" (others TBA)
#' @param path string, Path to the folder where data will be saved. Defaults to root
#' @return bool specifying success (TRUE) or failure (FALSE) of object transfers. Returns FALSE on first failure.
#'
#' @examples
#' \dontrun{
#' rwlab_gc_auth("your-email-address")
#' transfer_pod_data("Equity Factors", "./data")
#' }
transfer_pod_data <- function(pod, path = ".") {
  # TODO: check authorisation
  # TODO: check file exists in bucket

  pod_meta = get_pod_meta(pod) # datasets we want to use in the Pod
  googleCloudStorageR::gcs_global_bucket(pod_meta[["bucket"]])
  if(is.na(pod_meta[["essentials"]])) {
    cat("Pod not compatible with bulk transfer of data. Transfer objects individually.")
    return()
  }
  bucket_objects <- googleCloudStorageR::gcs_list_objects()  # datasets that exist in the Pod's bucket

  cat("Attempting download of ", pod_meta[["essentials"]], "...\n")
  for(dataset in pod_meta[["essentials"]]) {
    cat("Transferring", dataset, "... data is",  bucket_objects[bucket_objects$name == dataset, "size"], "please be patient...\n")
    if(googleCloudStorageR::gcs_get_object(dataset, saveToDisk = glue::glue("{path}/{dataset}"), overwrite = TRUE))
      cat(dataset, "successfully transferred\n")
    else {
      cat(dataset, "failed to transfer\n")
      return(FALSE)
    }
  }

  TRUE
}

#' Transfer a single specified object from data library
#'
#' This is useful for reloading a specific object rather than transferring all Research Pod objects.
#'
#' @param pod string, Name of the Research Pod
#' @param object string, Name of the object to transfer
#' @param path string, Local path for saving object, defaults to "."
#'
#' @return `bool` specifying success (TRUE) or failure (FALSE) of object transfer
#'
#' @examples
#' \dontrun{
#' transfer_lab_object(path = ".", object = "clean_R1000.csv", bucket = "rw_equity_research_sprint")
#' }
transfer_lab_object <- function(pod, object, path = ".") {
# TODO: check object exists, get file size
  # note that gcs_ functions already have good checks and error handling (eg check if object exists) so no need to  reinvent that wheel

  pod_meta = get_pod_meta(pod)

  # attempt object transfer
  if(googleCloudStorageR::gcs_get_object(
    object = object,
    bucket = pod_meta[["bucket"]],
    saveToDisk = glue::glue("{path}/{object}"),
    overwrite = TRUE
    )) {
    cat("File successfully transferred\n")
    TRUE
  } else {
    cat("File failed to transfer\n")
    FALSE
  }
}

#' Transfer object from data library and return it as a `data.frame`
#'
#' @param pod string, The name of the Research Pod
#' @param object string, The name of the data object
#' @param path string, The local path to the directory to save the data object. defaults to "."
#'
#' @return `object` as a `data.frame`
#' @export
#'
#' @examples
#' \dontrun{
#' load_lab_object("EquityFactors", "R1000_fundamentals_1d.feather")
#' }
load_lab_object <- function(pod, object, path = ".") {

  if(transfer_lab_object(pod = pod, object = object, path = path)) {
    feather::read_feather(glue::glue("{path}/{object}"))
  }
}

#' Transfers prices data from Research Pod data library and adds it as a `data.frame` to the Global Environment
#'
#' @param pod string, The name of the Research Pod
#' @param path string, The path to the local directory to save the transferred data
#'
#' @return nothing, but saves Research Pod price data locally and populates Global Env with a `data.frame`
#' @export
#'
#' @examples
#' \dontrun{
#' quickset("EquityFactors")
#' }
quicksetup <- function(pod, path = ".") {

  pod_meta <- get_pod_meta(pod)
  prices_file <- pod_meta[["prices"]]

  if(transfer_pod_data(pod, path = path)) {
    prices <- feather::read_feather(glue::glue("{path}/{prices_file}"))
    # Ensure date column is a date.
    prices$date <- lubridate::date(prices$date)
    assign("prices", prices, envir = .GlobalEnv)
    cat("prices data object transferred and loaded as data.frame to Global Env\n")
  } else {
    cat("Nothing added to Global Env\n")
  }

}
