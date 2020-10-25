#' List objects in a RW Lab Google Cloud bucket
#'
#' Requires authorisation. Set up prior with `rwlab_gc_auth`.
#'
#' @param bucket string, The name of the bucket
#'
#' @return A list of objects in `bucket`
#' @export
#'
#' @examples
#' \dontrun{
#' rwlab_gc_auth("your-email-address")
#' rwlab_list_bucket_objects("rw_equity_research_sprint")
#' }
rwlab_list_bucket_objects <- function(bucket = "rw_equity_research_sprint") {
  # TODO: if(.gcs_env$bucket != bucket) gcs_global_bucket(bucket)
  googleCloudStorageR::gcs_global_bucket(bucket)
  googleCloudStorageR::gcs_list_objects(bucket)
}
# TODO: replace with a function that sets global bucket according to a named list of Pod - bucket name


#' Transfer all data relevant to a specific research pod from Google Cloud and save to disk
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
  #TODO: get file size from gcs_list_objects??
  if(pod == "Equity Factors") {
    googleCloudStorageR::gcs_global_bucket("rw_equity_research_sprint")
    print("Transferring clean_R1000.csv ... data is 874MB please be patient...")
    if(googleCloudStorageR::gcs_get_object('clean_R1000.csv', saveToDisk = glue::glue("{path}/clean_R1000.csv"), overwrite = TRUE))
       print("file successfully transferred")
    else
      stop("Error: file failed to transfer")

    print("Transferring fundamentals.csv ... data is 360MB please be patient...")
    if(googleCloudStorageR::gcs_get_object('fundamentals.csv', saveToDisk = glue::glue("{path}/fundamentals.csv"), overwrite = TRUE))
      print("file successfully transferred")
    else
      stop("Error: file failed to transfer")
  } else {
    print("Specified Research Pod is unknown - nothing loaded")
  }
}

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

setup_for_pod <- function(pod, path = ".", oauth_email = NA) {
  "do everything needed to get going with research pod - auth and load data"
  rwlab_gc_auth(oauth_email = oauth_email)
  load_pod_data(pod, path)
}
