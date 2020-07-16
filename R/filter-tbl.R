# Filter peaks ------------------------------------------------------------

#' Filter out all samples that are not a peak
#'
#' Filter out all samples that are not a peak for purposes of plotting. The
#' length of each peak can either be left as is or normalized to a single
#' consistent length.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_delay
#' @param length_out Length of each peak. Default is `NULL`, which leaves the
#'   length of each peak as is. Use this argument to normalize the lengths of
#'   the peaks so that they are all of equal length. Use `peaks_length()` to
#'   check length of peaks to find good value.
#'
#' @return A tibble with two new columns: `sub_sample` provides the sample
#'   number within each peak and `peak_nr` identifies the peaks.
#'
#' @examples
#' x <- find_peaks_response(ex_trace_tbl)
#'
#' filter_peaks(ex_trace_tbl, x)
#'
#' # Normalize lengths of the peaks
#' peaks_length(x)
#'
#' filter_peaks(ex_trace_tbl, x, length_out = 250)
#'
#' @export

filter_peaks <- function(df, peaks, length_out = NULL) {

  if (!is.null(length_out)) {
    # Make each peak the same length
    # Lengths to add or subtract from each peak
    lengthen <- (length_out - peaks_length(peaks)) %/% 2
    # New start for each peak
    start <- purrr::map_int(peaks, min) - lengthen
    # Recreate peaks with vectors from start to length_out
    peaks <- purrr::map(start, ~ seq(from = .x, length.out = length_out))
  }

  # Filter out samples not in peaks and create two new columns
  purrr::map2_df(.x = peaks, .y = seq_along(peaks),
                 ~ df %>%
                   dplyr::filter(.data$sample %in% .x) %>%
                   dplyr::mutate(sub_sample = seq_along(.x),
                                 peak_nr = .y))
}


# Filter full response ----------------------------------------------------

#' Filter trace data frame by stiumli
#'
#' Filter out anything before the first stimulus and after the last stimulus.
#' This gets rid of all recordings before and after the performance of the
#' experiment. This is particularly useful for plotting the full trace.
#'
#' @inheritParams find_stimuli
#' @inheritParams find_peaks_stimulus
#' @param buffer Default is 100. Milliseconds to add before first stimulus
#'   and after the last stimulus.
#'
#' @seealso `filter_full_response()`
#'
#' @examples
#' filter_full_stimuli(ex_trace_tbl)
#'
#' @export

filter_full_stimuli <- function(df, buffer = 100,
                                stimulus_diff = 9000,
                                freq = 10000) {
  stimuli <- stimuli_samples(df, stimulus_diff)

  buffer_df(df, stimuli, buffer, freq)
}


#' Filter trace data frame by response
#'
#' Filter out anything before the first peak and after the last peak.
#' This gets rid of all recordings before and after the performance of the
#' experiment. This is particularly useful for plotting the full trace.
#'
#' @inheritParams filter_full_stimuli
#' @inheritParams peaks_checks
#'
#' @seealso `filter_full_stimuli()`
#'
#' @examples
#' x <- find_peaks_response(ex_trace_tbl)
#'
#' filter_full_response(ex_trace_tbl, x)
#'
#' @export

filter_full_response <- function(df, peaks, buffer = 100, freq = 10000) {
  # Beginning of first peak to end of last peak
  extent <- c(min(peaks[[1]]), max(peaks[[length(peaks)]]))

  buffer_df(df, extent, buffer, freq)
}
