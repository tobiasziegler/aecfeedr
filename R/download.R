#' List all files in an AEC feed directory
#'
#' Description goes here
#'
#' @param url A string containing the URL of the directory
#'
#' @return The contents of the directory
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
