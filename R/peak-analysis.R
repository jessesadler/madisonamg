## Analysis functions for peaks ##

#' Find length of peaks
#'
#' Find the length of peaks in either samples or milliseconds. This is useful
#' to help with functions that normalize the length of the peaks such as
#' `filter_peaks()`. It can also be used as part of an analysis of the peaks.
#'
#' @inheritParams peaks_checks
#' @param samples Whether to return the number of samples in the peaks, the
#'   default, or the length in milliseconds if set to `FALSE`.
#' @param Frequency of recording. Used to convert milliseconds to samples.
#'   Default is `NULL` and only used if `samples = FALSE`.
#'
#' @return A numeric vector showing the number of samples in each peak if
#'   `samples = TRUE`, or a vector showing the length of each peak in
#'   milliseconds if `samples = FALSE`.
#'
#' @examples
#' x <- find_peaks_response(ex_trace_tbl)
#'
#' # Find length of peaks in samples
#' peaks_length(x)
#'
#' # Find length of peaks in milliseconds
#' peaks_length(x, samples = FALSE, freq = 10000)
#'
#' @export

peaks_length <- function(peaks, samples = TRUE, freq = NULL) {
  # Checks
  check_samples(peaks)

  if (is.list(peaks)) {
    x <- purrr::map_int(peaks, length)
  } else {
    x <- length(peaks)
  }

  if (isFALSE(samples)) {
    if (is.null(freq)) {
      rlang::abort("<freq> must be provided if <samples> is FALSE.")
    }
    samples_to_ms(x, freq)
  } else {
    x
  }
}

#' Area under the curve of peaks
#'
#' Find the area under the curve for peaks of a trace tibble. Area under the
#' curve is calculated using trapezoidal integration provided by
#' `pracma::trapz()`. It uses the response amplitude as the `y` coordinate and
#' sample as the `x` coordinate.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_checks
#' @inheritParams create_report
#'
#' @return Return a numeric vector showing the area under the curve for the
#' peaks.
#'
#' @examples
#' x <- find_peaks_response(ex_trace_tbl)
#' peaks_auc(ex_trace_tbl, x)
#'
#' # Changing the baseline will affect area under the curve calculation
#' peaks_auc(ex_trace_tbl, x, baseline = 0)
#'
#' @export

peaks_auc <- function(df, peaks, baseline = NULL) {
  # Checks
  check_trace(df)
  check_samples(peaks)
  check_basline(baseline, peaks)

  # Find values
  values <- find_values(df, peaks)

  # Create baseline if NULL
  if (is.null(baseline)) {
    baseline <- purrr::map_int(values, min)
  }
  # Factor in the baseline
  values <- purrr::map2(values, baseline, ~ .x - .y)

  # Calculate area under the curve
  purrr::map2_dbl(peaks, values, ~ pracma::trapz(.x, .y))
}
