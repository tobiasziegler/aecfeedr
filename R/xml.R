#' Unpack the data from XML elements contained in a list-column
#'
#' @param data A data frame
#' @param xml_col The bare (unquoted) name of the list-column
#'   containing XML data
#' @param formula A formula specifying how to extract data from each XML node
#'
#' @return A data frame with the unpacked data
#' @export
#'
#' @examples
unpack_xml <- function(data, xml_col, formula) {
  out <- dplyr::mutate(
    data,
    unpacked_xml = purrr::map(
      {{ xml_col }},
      {{ formula }}
    )
  )

  out <- dplyr::select(out, -{{ xml_col }})

  out <- tidyr::unnest(out, cols = tidyselect::all_of(c("unpacked_xml")))

  out
}
