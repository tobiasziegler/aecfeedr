#' Get the URL for an AEC election feed directory
#'
#' Specify the parameters of the election feed you would like to access, and
#' this function will provide the URL for the corresponding FTP site and
#' directory.
#'
#' @param id The unique numeric ID of the electoral event
#' @param granularity Standard or Detailed granularity
#' @param verbosity Eml, Light, LightProgress, Preload or Verbose
#' @param archived FALSE for a live election feed, TRUE for a past election
#'
#' @return A string containing the URL
#' @export
get_directory_url <- function(id, granularity, verbosity, archived = FALSE) {
  # Live results feeds and archived results are on different FTP sites
  if (archived) {
    base_url <- "ftp://mediafeedarchive.aec.gov.au"
  } else {
    base_url <- "ftp://mediafeed.aec.gov.au"
  }

  # Join the directory structure to the base URL
  paste(base_url, id, granularity, verbosity, "/", sep = "/")
}

#' List all files in an AEC feed directory
#'
#' This helper function connects to the FTP site and returns the list of files
#' within the directory.
#'
#' @param url A string containing the URL of the directory
#'
#' @return A character vector containing the filenames within the directory
#' @export
list_files <- function(url) {
  # Set up the handle to configure the curl request
  handle <- curl::new_handle()
  curl::handle_setopt(handle = handle, dirlistonly = TRUE)

  # Retrieve the directory listing
  con <- curl::curl(url = url, handle = handle)
  files <- readLines(con)
  close(con)

  # Return the directory listing
  files
}
