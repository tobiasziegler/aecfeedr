#' Get the URL for an AEC election feed directory
#'
#' Description goes here
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
#' Description goes here
#'
#' @param id The unique numeric ID of the electoral event
#' @param granularity Standard or Detailed granularity
#' @param verbosity Eml, Light, LightProgress, Preload or Verbose
#' @param archived FALSE for a live election feed, TRUE for a past election
#'
#' @return The contents of the directory
#' @export
list_files <- function(id, granularity, verbosity, archived = FALSE) {
  # Get the URL for the directory
  url <- get_directory_url(id, granularity, verbosity, archived)

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
