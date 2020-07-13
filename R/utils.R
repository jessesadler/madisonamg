## Utility functions ##

#' Split vector of values by an index into a list
#'
#' Creates a list whose elements are split when the index values are
#' non-consecutive. Useful to turn tibble to list of numeric vectors.
#'
#' `values` and `index` will often be the same vector but need not be,
#' hence the use of two arguments.
#'
#' @param values Vector of values that will be returned.
#' @param index Vector of index to be used to split vector into list.
#'
#' @keywords internal

split_list <- function(values, index) {

  # Create "factor" for where to split values.
  # Split where the index is not consecutive. Use of cumsum() to label all
  # consecutive as same value. Begin with TRUE (1) and add 1 for each TRUE.
  split_factor <- cumsum(c(TRUE, diff(index) != 1L))

  out <- split(values, split_factor)

  # Remove names
  purrr::set_names(out, NULL)
}
