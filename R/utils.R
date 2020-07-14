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


#' Convert between milliseconds and samples at a given frequency
#'
#' @param x Value to convert.
#' @param freq Frequency of trace.
#'
#' @keywords internal
#'
#' @name ms_conversion
NULL

#' @rdname ms_conversion
ms_to_samples <- function(x, freq) {
  x * freq / 1000
}

#' @rdname ms_conversion
samples_to_ms <- function(x, freq) {
  x * 1000 / freq
}

#' Create a buffer data frame to lengthen a filtered trace
#'
#' Used in visualization functions.
#'
#' @param df A trace data frame.
#' @param extent A vector of samples for the extent of filtered trace to which
#'   the buffer is added. Usually this is where stimuli occur from
#'   `stimuli_samples()`.
#' @param buffer Buffer to add to the beginning and end of the plot in
#'   milliseconds.
#' @param freq Frequency of the trace recording.
#'
#' @keywords internal

buffer_df <- function(df, extent, buffer, freq) {

  buffer <- ms_to_samples(buffer, freq)
  start <- min(extent) - buffer
  end <- max(extent) + buffer

  df <- dplyr::filter(df, .data$sample >= start & .data$sample <= end)
}
