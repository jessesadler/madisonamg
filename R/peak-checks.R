# Checks for peak groups --------------------------------------------------

#' Checks for peak groups
#'
#' @description
#' Checks to help explore the peaks identified by `find_peaks_response()`,
#' `find_peaks_stimuli()`, or `find_peaks_manual()`.
#'
#' - `peaks_min_value()` finds the minimum value of each peak, essentially
#'   the smallest value above the baseline.
#' - `peaks_first_value()` finds the first value of each peak. This should
#'   correspond with, or at least be close to, the minimum and first value(s).
#' - `peaks_last_value()` finds the last value of each peak. This should
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
peaks_min_value <- function(df, peaks) {

  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), min)
  } else if (is.numeric(peaks)) {
    min(find_values(df, peaks))
  } else {
    stop(call. = FALSE,
         "<peaks> must by a numeric vector or list of numeric vectors")
  }
}

#' @rdname peaks_checks
#' @export
peaks_first_value <- function(df, peaks) {

  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), dplyr::first)
  } else if (is.numeric(peaks)) {
    dplyr::first(find_values(df, peaks))
  } else {
    stop(call. = FALSE,
         "<peaks> must by a numeric vector or list of numeric vectors")
  }
}

#' @rdname peaks_checks
#' @export
peaks_last_value <- function(df, peaks) {
  if (is.list(peaks)) {
    purrr::map_int(find_values(df, peaks), dplyr::last)
  } else if (is.numeric(peaks)) {
    dplyr::last(find_values(df, peaks))
  } else {
    stop(call. = FALSE,
         "<peaks> must by a numeric vector or list of numeric vectors")
  }
}


#' Check delay between peaks and stimuli
#'
#' This function is meant to be used to help check for any problems in finding
#' the peaks via `find_peaks_response()` or `find_peaks_stimulus()`. It shows
#' the number of samples between each stimulus and the response peak.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_checks
#' @param min_delay Default 0. Minimum number of samples that the peaks should
#'   begin after the stimuli. A warning is given if the value is less.
#' @param max_delay Default 200. Maximum number of samples that the peaks
#'   should begin after the stimuli. A warning is given if the value is more.
#'
#' @export

peaks_delay <- function(df, peaks, stimulus_diff = 9000,
                        min_delay = 0, max_delay = 200) {
  stimuli <- stimuli_samples(df, stimulus_diff)

  if (length(stimuli) != length(peaks)) {
    stop(call. = FALSE,
         glue::glue("Number of stimuli found ({length(stimuli)}) is different
                     from number of responses ({length(peaks)}) in <peaks>."))
  }

  # Find delay between stimulus and peak
  delays <- purrr::map_dbl(peaks, min) - stimuli

  # Less than min delay
  if (any(delays < min_delay)) {
    mistakes <- which(delays < min_delay)
    mistakes <- glue::glue_collapse({mistakes}, sep = ", ", last = ", and ")

    warning(call. = FALSE,
            glue::glue("Peaks {mistakes} begin less than {min_delay} samples",
                       " after the stimulus."))
  }

  # More than max delay
  if (any(delays > max_delay)) {
    mistakes <- which(delays > max_delay)
    mistakes <- glue::glue_collapse({mistakes}, sep = ", ", last = ", and ")

    warning(call. = FALSE,
            glue::glue("Peaks {mistakes} begin more than {max_delay} samples",
                       " after the stimulus."))
  }

  delays
}

#' Check length of peaks
#'
#' Helper function to check the number of samples in each peak. This is useful
#' to help with functions that normalize the length of the peaks such as
#' `filter_peaks()`.
#'
#' @inheritParams peaks_checks
#'
#' @export

peaks_length <- function(peaks) {
  if (!is.list(peaks)) {
    stop(call. = FALSE, "<peaks> must be a list of numeric vectors.")
  }

  purrr::map_int(peaks, length)
}
