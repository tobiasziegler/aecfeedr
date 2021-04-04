#' Get the URL for an AEC election feed directory
#'
#' Specify the parameters of the election feed you would like to access, and
#' this function will provide the URL for the corresponding FTP site and
#' directory.
#'
#' See the [AEC Media Feed User Guide](https://www.aec.gov.au/media/mediafeed/)
#' for details of the contents of each feed type and the available combinations
#' of granularity and verbosity.
#'
#' @param id The unique numeric ID of the electoral event
#' @param granularity Standard or Detailed granularity
#' @param verbosity Eml, Light, LightProgress, Preload or Verbose
#' @param archived FALSE for a live election feed, TRUE for a past election
#'
#' @examples
#' feed_get_url(25881, "Detailed", "Preload", TRUE)
#' feed_get_url(25881, "Detailed", "Light", TRUE)
#'
#' @return A string containing the URL
#' @export
feed_get_url <- function(id,
                         granularity = c(
                           "Detailed",
                           "Standard"
                         ),
                         verbosity = c(
                           "Light",
                           "Eml",
                           "LightProgress",
                           "Preload",
                           "Verbose"
                         ),
                         archived = FALSE) {
  granularity <- rlang::arg_match(granularity)
  verbosity <- rlang::arg_match(verbosity)
  if ((verbosity == "Eml" && granularity == "Detailed") || (verbosity == "LightProgress" && granularity == "Standard")) {
    rlang::abort(feed_combo_invalid_msg(granularity, verbosity), class = "aecfeedr_error")
  }

  # Live results feeds and archived results are on different FTP sites
  if (archived) {
    base_url <- "ftp://mediafeedarchive.aec.gov.au"
  } else {
    base_url <- "ftp://mediafeed.aec.gov.au"
  }

  # Join the directory structure to the base URL
  glue::glue("{base_url}/{id}/{granularity}/{verbosity}/")
}

feed_combo_invalid_msg <- function(verbosity, granularity) {
  msg <- glue::glue("
                    The AEC does not provide feeds with `{granularity}` granularity and `{verbosity}` verbosity.
                    Check the Media Feed User Guide (https://www.aec.gov.au/media/mediafeed/) for available combinations.
                    ")
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
feed_list_files <- function(url) {
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

#' Download an AEC feed file and extract its contents
#'
#' This function connects to the FTP site, downloads a zip file from a feed
#' directory, and extracts the XML message(s) and metadata contained within.
#'
#' @param dir_url A string containing the URL of the directory
#' @param filename A string containing the name of the zip file to download
#' @param destpath A string specifying the base path to download and extract
#'   files in
#'
#' @return A character vector of the filepaths extracted to, invisibly
#' @export
feed_get_messages <- function(dir_url, filename, destpath) {
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop(
      "Package \"tools\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # Download the zip file
  url <- paste(dir_url, filename, sep = "")
  zip <- file.path(destpath, filename)
  curl::curl_download(url, zip)

  # Extract the XML message(s), schemas, etc., contained in the zip file to a
  # subdirectory labelled with the zip filename (incl. timestamp)
  unzip_dir <- file.path(destpath, tools::file_path_sans_ext(filename))
  utils::unzip(zip, exdir = unzip_dir)
}
