#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Unpack the data from XML elements contained in a list-column
#'
#' Some elements within AEC feeds contain a value of 0 when results haven't
#' been received yet. This function checks whether each full group of vote
#' count elements (e.g., per polling place) has zero values and, if so, changes
#' them all to NA. Provide grouping variables via the dots (...) in the
#' function call.
#'
#' @param x A data frame
#' @param ... Grouping variables that will be applied to identify whether a
#'   complete group of rows have zero total votes reporting.
#'
#' @return A data frame with zeroes replaced by `NA`s
#' @export
replace_zeroes_with_na <- function(x, ...) {
  grouping <- rlang::quos(...)

  if (nrow(x) > 0) {
    x <- x %>%
      dplyr::group_by(!!!grouping) %>%
      dplyr::mutate(
        total_votes = sum(.data$votes),
        votes = dplyr::if_else(
          .data$total_votes > 0,
          .data$votes,
          NA_integer_,
          NA_integer_
        )
      ) %>%
      dplyr::select(-.data$total_votes) %>%
      dplyr::ungroup()
  }
  x
}
