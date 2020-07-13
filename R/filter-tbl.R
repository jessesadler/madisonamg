# Filter peaks ------------------------------------------------------------

#' Filter out all samples that are not a peak
#'
#' `filter_peaks()` is useful to plot only the peaks from a trace.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_delay
#' @param length_out Default is 200. The length of each peak in samples. Use
#'   `peaks_length()` to check length of peaks to find good value.
#'
#' @return A tibble with rows equal to `length_out * length(peaks)` and six
#'   columns. `filter_peaks()` creates two new columns: a `sub_sample` column
#'   that is `1:length_out` for each peak and a `peak_nr` to identify each
#'   peak.
#'
#' @examples
#' x <- find_peaks_response(ex_trace_tbl)
#'
#' peaks_length(x)
#'
#' filter_peaks(ex_trace_tbl, x, 250)
#'
#' @export

filter_peaks <- function(df, peaks, length_out = 200) {
  peak_nr <- seq_along(peaks)
  # Figure out the number of samples to be added to each group to equal length_out
  lengthen <- (length_out - peaks_length(peaks)) %/% 2

  # Use below function to perform filtering and row bind with map2_df()
  purrr::map2_df(peak_nr, lengthen, ~ .filter_peaks(df, peaks, .x, .y, length_out))
}

# Function to perform the filtering and mutate
.filter_peaks <- function(df, peaks, peak_nr, lengthen, length_out) {
  df %>%
    dplyr::filter(sample %in% seq(min(peaks[[peak_nr]]) - lengthen,
                                  length.out = length_out)) %>%
    dplyr::mutate(sub_sample = seq(length_out),
                  peak_nr = peak_nr)
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
                                stimulus_diff = 9000, freq = 10000) {
  stimuli <- stimuli_samples(df = df, stimulus_diff = stimulus_diff)

  start <- min(stimuli) - buffer * freq / 1000
  end <- max(stimuli) + buffer * freq / 1000

  dplyr::filter(df, sample >= start & sample <= end)
}


#' Filter data frame by response
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

  start <- min(peaks[[1]]) - buffer * freq / 1000
  end <- max(peaks[[length(peaks)]]) + buffer * freq / 1000

  dplyr::filter(df, sample >= start & sample <= end)
}
