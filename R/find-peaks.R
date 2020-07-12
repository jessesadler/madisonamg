#' Find peaks by amplitude of response
#'
#' Find peaks by looking for when response is above a set `min_amp` for an
#' extended period of time (`min_length`). The function then finds all samples
#' above the `baseline`.
#'
#' @seealso See [`find_peaks_stimulus()`] for another way to calculate the
#' beginning and ending of peaks. Use [`peaks_delay()`] to check the output.
#'
#' @inheritParams find_stimuli
#' @param baseline Baseline of signal. Function will find peaks above this
#'   value using greater than. Can be vector of length 1 or length of the
#'   number of peaks.
#' @param min_amp Minimum amplitude of response to find the peaks. This is
#'   used first to find the placement of the peaks.
#' @param min_length Minimum number of samples that should be above `min_amp`
#'   in the peaks. This helps to find the peaks and distinguish peaks
#'   in response to stimuli from noise.
#' @param lengthen Number of samples to lengthen the beginning and end of the
#'   peak from the `min_amp` value to reach the `baseline`. If this number is
#'   too large and the `baseline` is too small, the peak might start before
#'   the stimulus. If this number is too small, it might not reach the
#'   `baseline`.
#'
#' @return Returns a list of the length of the number of peaks. Each list
#'   contains the samples of each peak. These are not the recorded values
#'   but where the peaks occur on the trace.
#'
#' @export

find_peaks_response <- function(df,
                                baseline = 256,
                                min_amp = 1500,
                                min_length = 50,
                                lengthen = 100) {
  lengthen <- round(lengthen / 2)

  filtered_tbl <- dplyr::filter(df, .data$response > min_amp)

  response_list <- split(filtered_tbl$sample,
                         cumsum(c(TRUE, diff(filtered_tbl$sample) != 1L))) %>%
    purrr::set_names(NULL) %>%
    purrr::keep(~ length(.x) > min_length)

  expanded_list <- purrr::map(response_list,
                              ~ seq(min(.) - lengthen, max(.) + lengthen))
  vals_list <- find_values(df, expanded_list)
  above_base <- purrr::map2(vals_list, baseline, ~ .x > .y) %>%
    purrr::flatten_lgl()

  response_vctr <- purrr::flatten_int(expanded_list)[above_base]

  out <- split(response_vctr,
               cumsum(c(TRUE, diff(response_vctr) != 1L))) %>%
    purrr::set_names(NULL) %>%
    purrr::keep(~ length(.x) > min_length)

  out
}

#' Find peaks by specified delay from stimuli
#'
#' Find peaks by finding the stimuli with `find_stimuli()` and then beginning
#' the peak a set number of milliseconds after the onset of the stimulus. The
#' `baseline` can either be defined by the user or decided separately for each
#' peak based on the value of the response after the given delay. This latter
#' method is the default.
#'
#' @seealso See [`find_peaks_response()`] for another way to calculate the
#' beginning and ending of peaks. Use [`peaks_delay()`] to check the output.
#'
#' @inheritParams find_stimuli
#' @param delay Delay in milliseconds from the onset of the stimulus to begin the
#'   peak.
#' @param freq Frequency of recording. Used to convert milliseconds to samples.
#' @param baseline Baseline of signal. Default is `NA`, which chooses the
#'   `baseline` for each peak based on the value of the response at the
#'   given `delay` from the stimulus. Function will find peaks above this
#'   value using greater than. Can be vector of length 1 or length of the
#'   number of peaks.
#' @param min_length Minimum length of the peak in samples. This helps to
#'   ensure measurement of peak does not end too early.
#'
#' @return Returns a list that should be the length of the number of peaks.
#'   Each list contains the samples of each peak. These are not the recorded
#'   values but where the peaks occur on the trace. Make sure to check that
#'   the length of the returned list is the same as the number of stimuli. If
#'   this is not the case, arguments will need to be changed.
#'
#' @export

find_peaks_stimulus <- function(df, delay,
                                freq = 10000,
                                stimulus_diff = 9000,
                                baseline = NA,
                                min_length = 50) {
  # Stimuli
  stimuli <- stimuli_samples(df = df, stimulus_diff = stimulus_diff)

  # start of response
  start <- stimuli + delay * freq / 1000

  # Bases at start
  if (is.na(baseline)) {
    baseline <- df %>%
      dplyr::filter(sample %in% start) %>%
      dplyr::pull(.data$response)
  }

  # Find end of response

  # Minimum end of response
  # Adding min_length helps to avoid situations in which
  # first couple of values might be below baseline
  min_end <- start + min_length

  # Max end is before next stimulus
  # Want the one before FALSE is detected (hence - 1)
  max_end <- start + min(start - dplyr::lag(start), na.rm = TRUE)

  # Vector of possible samples: From min_end to max_end
  possible_vctr <- purrr::map2(min_end, max_end, seq) %>%
    purrr::flatten_int()

  # Find values
  filtered_df <- dplyr::filter(df, sample %in% possible_vctr)
  # List of response values
  vals_list <- split(filtered_df$response, cumsum(c(TRUE, diff(filtered_df$sample) != 1L))) %>%
    purrr::set_names(NULL)

  # Which are above baseline
  above_base <- purrr::map2(vals_list, baseline, ~ .x > .y)

  # Detect index of first FALSE
  find_end <- purrr::map_int(above_base, ~ purrr::detect_index(., isFALSE)) - 1

  # End is from start of index (min_end) to
  # Subtract 1 from min_end, so first value is not counted twice
  end <- min_end - 1 + find_end

  # Create groups of samples from start to end of peak
  purrr::map2(start, end, seq)
}
