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

#' Retrieve an attribute from the first node that matches an xpath expression
#'
#' This is a helper function that uses `xml_find_first` to retrieve a node and
#' then retrieves the specified attribute on that node.
#'
#' @param x A document, node or node set
#' @param xpath A string containing a xpath (1.0) expression
#' @param attr Name of attribute to extract
#' @param ns Optionally, a named vector giving prefix-url pairs, as produced
#'   by [xml_ns()]. If provided, all names will be explicitly
#'   qualified with the ns prefix, i.e. if the element `bar` is defined
#'   in namespace `foo`, it will be called `foo:bar`. (And
#'   similarly for attributes). Default namespaces must be given an explicit
#'   name. The ns is ignored when using [xml_name<-()] and
#'   [xml_set_name()].
#'
#' @return A character vector. `NA` is used to represent of attributes that
#'   aren't defined.
#' @export
#'
#' @examples
#' x <- xml2::read_xml("<a id='foo' />")
#' xml_find_attr(x, "/a", "id")
xml_find_attr <- function(x, xpath, attr, ns = xml_ns(x)) {
  xml_attr(
    xml_find_first(x, xpath, ns),
    attr
  )
}

#' Extract the text from the first node that matches an xpath expression
#'
#' This is a helper function that uses `xml_find_first` to retrieve a node and
#' then extracts the text from that node.
#'
#' @param x A document, node or node set
#' @param xpath A string containing a xpath (1.0) expression
#' @param trim If `TRUE` will trim leading and trailiing spaces
#' @param ns Optionally, a named vector giving prefix-url pairs, as produced
#'   by [xml_ns()]. If provided, all names will be explicitly
#'   qualified with the ns prefix, i.e. if the element `bar` is defined
#'   in namespace `foo`, it will be called `foo:bar`. (And
#'   similarly for attributes). Default namespaces must be given an explicit
#'   name. The ns is ignored when using [xml_name<-()] and
#'   [xml_set_name()].
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- xml2::read_xml("<a>foo</a>")
#' xml_find_text(x, "/a")
xml_find_text <- function(x, xpath, trim = FALSE, ns = xml_ns(x)) {
  xml_text(
    xml_find_first(x, xpath, ns),
    trim
  )
}
