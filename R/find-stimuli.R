# stimulus_frequency ------------------------------------------------------

#' Find when stimuli were performed
#'
#' Find when stimuli were performed by change in recorded value of stimulus
#' column from previous sample.
#'
#' @param df Data frame or tibble with sample, stimulus, and response columns.
#' @param stimulus_diff The stimulus is found by seeing when the stimulus
#'   recording increases by a given amount from the previous sample. Default is
#'   9000, which seems to work for most recordings.
#'
#' @return Returns a data frame in which each row shows the onset of the
#'   stimulus.
#'
#' @export

find_stimuli <- function(df, stimulus_diff = 9000) {
  df %>%
    dplyr::mutate(diff = .data$stimulus - dplyr::lag(.data$stimulus)) %>%
    dplyr::filter(diff > stimulus_diff) %>%
    dplyr::filter(.data$sample - dplyr::lag(.data$sample, default = 0) != 1)
}

# Return a tibble of the onset of the stimulus
# Difference is shown in either number of samples (sample = TRUE)
# or in seconds (sample = FALSE)

#' Find stimuli and frequency of stimuli
#'
#' Useful to find and check frequency of the stimuli by creating a `diff`
#' column to show time difference between stimuli by either sample or in
#' seconds.
#'
#' @inheritParams find_stimuli
#' @param sample Default TRUE. Should the diff column show difference in
#'   samples or in seconds.
#'
#' @seealso `find_stimuli()`
#'
#' @return This extends `find_stimuli` to show the difference between
#'   stimuli in terms of samples or seconds.
#'
#' @export

stimulus_frequency <- function(df, stimulus_diff = 9000, sample = TRUE) {

  if (sample == TRUE) {
    df %>%
      find_stimuli(stimulus_diff = stimulus_diff) %>%
      dplyr::mutate(diff = .data$sample - dplyr::lag(.data$sample))
  } else {
    df %>%
      find_stimuli(stimulus_diff = stimulus_diff) %>%
      dplyr::mutate(diff = .data$secs - dplyr::lag(.data$secs))
  }
}
