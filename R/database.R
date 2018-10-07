#' Write data processed from a feed into a database
#'
#' Specify the parameters of the election feed you would like to access, and
#' this function will provide the URL for the corresponding FTP site and
#' directory.
#'
#' @param conn A `DBIConnection`` object, as returned by `dbConnect()`
#' @param data A list of tibbles containing data from an AEC feed
#' @param ... Other parameters passed on to the `dbWriteTable` method
#'
#' @return Invisibly returns `data`
#' @export
write_db_tables <- function(conn, data, ...) {
  purrr::iwalk(
    data,
    ~ DBI::dbWriteTable(
      conn = conn,
      name = .y,
      value = .x,
      ...
    ),
    conn,
    ...
  )
}
