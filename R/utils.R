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

# Some elements within AEC feeds contain a value of 0 when results haven't been
# received yet. This function checks whether each full group of vote count
# elements (e.g., per polling place) has zero values and, if so, changes them
# all to NA. Provide grouping variables via the dots (...) in the function
# call.
replace_zeroes_with_na <- function(x, ...) {
  grouping <- rlang::quos(...)

  x %>%
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
