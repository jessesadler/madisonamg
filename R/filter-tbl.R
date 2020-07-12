# Filter peaks ------------------------------------------------------------

#' Filter out all samples that are not a peak
#'
#' Use to make plot with just peaks
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_delay
#' @param length_out Default is 200. The length of each peak in samples.
#'
#' @export

filter_peaks <- function(df, peaks, length_out = 200) {
  peak_nr <- seq_along(peaks)
  # Figure out the number of samples to be added to each group to equal length_out
  lengthen <- (length_out - purrr::map_dbl(peaks, length)) %/% 2

  purrr::map2_df(peak_nr, lengthen, ~ .filter_peaks(df, peaks, .x, .y, length_out))
}

.filter_peaks <- function(df, peaks, peak_nr, lengthen, length_out) {
  df %>%
    dplyr::filter(sample %in% seq(min(peaks[[peak_nr]]) - lengthen,
                                  length.out = length_out)) %>%
    dplyr::mutate(sub_sample = seq(length_out),
                  peak_nr = peak_nr)
}


# Filter full response ----------------------------------------------------

#' Filter data frame by stiumli
#'
#' Filter out anything before the first stimulus and after the last stimulus.
#'
#' Used to plot full response.
#'
#' @inheritParams find_stimuli
#' @inheritParams find_peaks_stimulus
#' @param buffer Default is 100. Milliseconds to add before first stimulus
#'   and after the last stimulus.
#'
#' @seealso `filter_full_response()`
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
#' Filter out anything before the beginning of the first peak and after
#' the end of the last peak.
#'
#' Used to plot full response.
#'
#' @inheritParams filter_full_stimuli
#' @inheritParams peaks_checks
#'
#' @seealso `filter_full_stimuli()`
#'
#' @export

filter_full_response <- function(df, peaks, buffer = 100, freq = 10000) {

  start <- min(peaks[[1]]) - buffer * freq / 1000
  end <- max(peaks[[length(peaks)]]) + buffer * freq / 1000

  dplyr::filter(df, sample >= start & sample <= end)
}
