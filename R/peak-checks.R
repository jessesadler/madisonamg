# Checks for peak groups --------------------------------------------------

#' Checks for peak groups
#'
#' @description
#' Checks to help explore the peaks identified by `find_peaks_response()`,
#' `find_peaks_stimuli()`, or `find_peaks_manual()`.
#'
#' - `peaks_min_amp()` finds the minimum value of each peak, essentially
#'   the smallest value above the baseline.
#' - `peaks_max_amp()` finds the maximum value of each peak.
#' - `peaks_first_amp()` finds the first value of each peak. This should
#'   correspond with, or at least be close to, the minimum and first value(s).
#' - `peaks_last_amp()` finds the last value of each peak. This should
#'   correspond with, or at least be close to, the minimum and first value(s).
#'
#' The values may differ slightly because the values from the recordings are
#' discrete and not continuous.
#'
#' @inheritParams find_stimuli
#' @param peaks List of numeric vectors with the samples for where the peaks
#'   occur.
#'
#' @return A vector of values
#'
#' @name peaks_checks
NULL

#' @rdname peaks_checks
#' @export
peaks_min_amp <- function(df, peaks) {
  # Checks
  check_trace(df)
  check_samples(peaks)

  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), min)
  } else {
    min(find_values(df, peaks))
  }
}

#' @rdname peaks_checks
#' @export
peaks_max_amp <- function(df, peaks) {
  # Checks
  check_trace(df)
  check_samples(peaks)

  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), max)
  } else {
    max(find_values(df, peaks))
  }
}

#' @rdname peaks_checks
#' @export
peaks_first_amp <- function(df, peaks) {
  # Checks
  check_trace(df)
  check_samples(peaks)

  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), dplyr::first)
  } else {
    dplyr::first(find_values(df, peaks))
  }
}

#' @rdname peaks_checks
#' @export
peaks_last_amp <- function(df, peaks) {
  # Checks
  check_trace(df)
  check_samples(peaks)

  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), dplyr::last)
  } else {
    dplyr::last(find_values(df, peaks))
  }
}


#' Check delay between peaks and stimuli
#'
#' This function is meant to be used to help check for any problems in finding
#' the peaks via `find_peaks_response()` or `find_peaks_stimulus()`. It shows
#' the number of milliseconds between each stimulus and the response peak.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_checks
#' @inheritParams find_peaks_stimulus
#' @param min_delay Default 0. Minimum number of milliseconds that the peaks
#'   should begin after the stimuli. A warning is given if the value is less.
#' @param max_delay Default 200. Maximum number of milliseconds that the peaks
#'   should begin after the stimuli. A warning is given if the value is more.
#'
#' @return A numeric vector of delays between the stimulus and the beginning of
#'   each peak in milliseconds.
#'
#' @export

peaks_delay <- function(df, peaks,
                        stimulus_diff = 9000,
                        freq = 10000,
                        min_delay = 0, max_delay = 20) {
  # Checks
  check_trace(df)
  check_samples(peaks)

  stimuli <- stimuli_samples(df, stimulus_diff)
  check_lengths(stimuli, peaks)

  min_samples <- ms_to_samples(min_delay, freq)
  max_samples <- ms_to_samples(max_delay, freq)

  # Find delay between stimulus and peak
  delays <- purrr::map_dbl(peaks, min) - stimuli

  # Less than min delay
  if (any(delays < min_samples)) {
    mistakes <- which(delays < min_samples)
    mistakes <- glue::glue_collapse({mistakes}, sep = ", ", last = " and ")

    rlang::warn(glue::glue("Peak(s) {mistakes} begin less than {min_delay}",
                           " milliseconds after the stimulus."))
  }

  # More than max delay
  if (any(delays > max_samples)) {
    mistakes <- which(delays > max_samples)
    mistakes <- glue::glue_collapse({mistakes}, sep = ", ", last = " and ")

    rlang::warn(glue::glue("Peak(s) {mistakes} begin more than {max_delay}",
                           " milliseconds after the stimulus."))
  }
  # Convert to ms
  samples_to_ms(delays, freq)
}
