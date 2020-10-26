#' Get Research Pod metadata
#'
#' Returns bucket and dataset names for a specified pod,
#' or all pods if pod is unspecified.
#'
#' @param pod string, The name of the Research Pod. If NA, data for all pods is returned.
#'
#' @return List of metadata for the specified Research Pod, or list of lists of metadata for all Pods if `pod` is NA.
#' @export
#'
#' @examples
#' bucket = rwlab_get_pod_meta("EquityFactors")[["bucket"]]
#' datasets = rwlab_get_pod_meta("EquityFactors")[["datasets"]]
#' all_pods_meta = rwlab_get_pod_meta()
rwlab_get_pod_meta <- function(pod = NA) {
  # intent is to make explicit the data to be used in a Research Pod
  pod_meta <- list(
    EquityFactors = list(
      bucket = "rw_equity_research_sprint",
      datasets = c("clean_R1000.csv", "fundamentals.csv")
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

list_pods <- function() {

}

#' Transfer all data relevant to a specific Research Pod from Google Cloud and save to disk
#'
#' Requires authorisation. Set up prior with `rwlab_gc_auth`.
#'
#' @param pod string, The name of the research pod. Options: "Equity Factors" (others TBA)
#' @param path string, Path to the folder where data will be saved. Defaults to root
#' @return None, transfers data from Google Cloud to `path`
#' @export
#'
#' @examples
#' \dontrun{
#' rwlab_gc_auth("your-email-address")
#' load_pod_data("Equity Factors", "./data")
#' }
load_pod_data <- function(pod, path = ".") {
  # TODO: check authorisation
  # TODO: check file exists in bucket

  pod_meta = rwlab_get_pod_meta(pod) # datasets we want to use in the Pod
  googleCloudStorageR::gcs_global_bucket(pod_meta[["bucket"]])
  bucket_objects <- googleCloudStorageR::gcs_list_objects()  # datasets that exist in the Pod's bucket

  cat("Attempting download of ", pod_meta[["datasets"]], "...\n")
  for(dataset in pod_meta[["datasets"]]) {
    cat("Transferring", dataset, "... data is",  bucket_objects[bucket_objects$name == dataset, "size"], "please be patient...\n")
    if(googleCloudStorageR::gcs_get_object(dataset, saveToDisk = glue::glue("{path}/{dataset}"), overwrite = TRUE))
      cat("file successfully transferred\n")
    else
      stop("Error: file failed to transfer")
  }
}

# todo: expose and document
load_data_object <- function(path, object, bucket = NA) {
  "load a single specified data object"
# TODO: check object exists, get file size
  # note that gcs_ functions already have good checks and error handling (eg check if object exists) so no need to  reinvent that wheel

  # set bucket
  if(is.na(bucket))
    bucket = googleCloudStorageR::gcs_get_global_bucket()

  # attempt object transfer
  if(googleCloudStorageR::gcs_get_object(object = object, bucket = bucket, saveToDisk = glue::glue("{path}/object"), overwrite = TRUE))
    print("file successfully transferred")
  else
    stop("Error: file failed to transfer")
}

# todo: expose and documetn this functin
setup_for_pod <- function(pod, path = ".", oauth_email = NA) {
  "do everything needed to get going with research pod - auth and load data"
  rwlab_gc_auth(oauth_email = oauth_email)
  load_pod_data(pod, path)
}
