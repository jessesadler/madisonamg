# stimulus_frequency ------------------------------------------------------

#' Find when stimuli were performed
#'
#' Find when stimuli were performed by change in recorded amplitude of the
#' stimulus column from the previous sample.
#'
#' @details
#' `find_stimuli()` finds the onset of stimuli and creates three new columns:
#'
#' - `amp_diff` shows the difference in the stimulus amplitude from the
#'   previous sample. This is useful for checking that the function properly
#'   found the stimuli.
#' - `sample_diff` shows the time difference between stimuli as samples.
#' - `sec_diff` shows the time difference between stimuli as seconds. These
#'   variables are useful for finding or checking the frequency of the stimuli
#'   and for finding any oddities in the data.
#'
#' @param df Data frame or tibble with sample, stimulus, and response columns.
#' @param stimulus_diff The stimulus is found by seeing when the amplitude of
#'   the stimulus recording increases by a given amount from the previous
#'   sample. Default is 9000, which seems to work for most recordings.
#'
#' @return Returns a data frame in which each row shows the onset of the
#'   stimulus. Adds `amp_diff`, `samp_diff`, and `sec_diff` columns.
#'
#' @examples
#'
#' find_stimuli(ex_trace_tbl)
#'
#' # If stimulus_diff is too high or low,
#' # you will get incorrect number of stimuli
#'
#' # Too high
#' find_stimuli(ex_trace_tbl, stimulus_diff = 30000)
#'
#' # Too low
#' find_stimuli(ex_trace_tbl, stimulus_diff = 300)
#'
#' @export

find_stimuli <- function(df, stimulus_diff = 9000) {

  # Filter to find stimuli
  filtered <- stimuli_filter(df = df, stimulus_diff = stimulus_diff)

  # Add sample_diff and sec_diff columns
  filtered %>%
    dplyr::mutate(sample_diff = .data$sample - dplyr::lag(.data$sample),
                  sec_diff = .data$secs - dplyr::lag(.data$secs))
}


# Internal find_stimuli functions -----------------------------------------

#' Filter trace tibble to find onset of stimuli
#'
#' Used in `find_stimuli`. Making this its own function simplifies its
#' internal use elsewhere.
#'
#' @inheritParams find_stimuli
#'
#' @return A tibble equivalent to find_stimuli() but without `sample_diff`
#'   and `sec_diff` columns.
#'
#' @keywords internal


stimuli_filter <- function(df, stimulus_diff) {
  # Create a variable to record amplitude difference over each sample.
  # Only keep rows that have an amplitude difference greater than stimulus_diff.
  # Get rid of any rows that immediately follow from the previous sample.

  df %>%
    dplyr::mutate(amp_diff = .data$stimulus - dplyr::lag(.data$stimulus)) %>%
    dplyr::filter(.data$amp_diff > stimulus_diff) %>%
    dplyr::filter(.data$sample - dplyr::lag(.data$sample, default = 0) != 1)
}

#' Find samples of stimuli
#'
#' Simplifies the process of find_stimuli() %>% dplyr::pull(sample)
#'
#' @inheritParams find_stimuli
#'
#' @return Vector of the samples where onset of stimuli occur.
#'
#' @keywords internal

stimuli_samples <- function(df, stimulus_diff) {
  stimuli_filter(df = df, stimulus_diff = stimulus_diff)[["sample"]]
}
