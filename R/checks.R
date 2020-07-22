## Checks for functions ##

#' Check trace tibble
#'
#' Check that `df` is a data frame. That it has variables named `sample`,
#' `stimulus`, and `response`. That these are numeric.
#'
#' @param df A trace tibble to check.
#'
#' @keywords internal

check_trace <- function(df) {
  if (!is.data.frame(df)) {
    rlang::abort(
      glue::glue("<df> must be a data frame with variables named `sample`,",
                 " `stimulus`, and `response`, all of which must be numeric."))
  }
  # Has necessary variables with correct names
  if (!all(c("sample", "stimulus", "response") %in% names(df))) {
    rlang::abort(
      "<df> must have variables named `sample`, `stimulus`, and `response`.")
  }
  if (!is.numeric(df$sample)) {
    rlang::abort("`sample` must be a numeric vector.")
  }

  if (!is.numeric(df$stimulus)) {
    rlang::abort("`stimulus` must be a numeric vector.")
  }

  if (!is.numeric(df$response)) {
    rlang::abort("`response` must be a numeric vector.")
  }
}

#' Check samples is numeric
#'
#' Checks that `x` is either a list of numeric vectors or a numeric vector.
#' Provides different message whether it is used to check `peaks` or `samples`.
#'
#' @param x Object to check.
#' @param peaks Whether checked argument is peaks or samples.
#'
#' @keywords internal

check_samples <- function(x, peaks = TRUE) {

  # Should error message be about samples or peaks
  if (peaks) {
    type <- "peaks"
  } else {
    type <- "samples"
  }
  msg <- glue::glue("<{type}> must be a numeric vector or list of numeric vectors.")

  if (is.list(x)) {
    if (!all(purrr::map_lgl(x, is.numeric))) {
      rlang::abort(msg)
    }
  } else {
    if (!is.numeric(x)) {
      rlang::abort(msg)
    }
  }
}

#' Check that stimuli and peaks are the same length
#'
#' Check that there are the same number of stimuli found as peaks.
#'
#' @param stimuli A numeric vector of stimuli from `stimuli_samples()`.
#' @param peaks A list of numeric vectors or a numeric vectors of samples.
#'
#' @keywords internal

check_lengths <- function(stimuli, peaks) {

  # If peaks is a list
  if (is.list(peaks)) {
    if (length(stimuli) != length(peaks)) {
      rlang::abort(
        glue::glue("Number of stimuli found ({length(stimuli)})",
                   " is different from peaks ({length(peaks)}).
                    Check that <peaks> is correct or alter <stimulus_diff>",
                   " to correct the number of stimuli found."))
    }
  } else {
    # If peaks is a numeric vector
    if (length(stimuli) != 1L) {
      rlang::abort(
        glue::glue("Number of stimuli found ({length(stimuli)})",
                   " is different from peaks (1).
                    Check that <peaks> is correct or alter <stimulus_diff>",
                   " to correct the number of stimuli found."))
    }
  }
}

#' Check baseline
#'
#' Check that baseline is either NULL, a numeric vector of length 1, or of
#' length equal to the length of peaks.
#'
#' @param baseline Baseline amplitude to be used for calculations.
#' @param peaks A list of numeric vectors or a numeric vectors of samples.
#'
#' @keywords internal

check_basline <- function(baseline, peaks) {
  if (!is.null(baseline)) {
    if (!is.numeric(baseline)) {
      rlang::abort("<baseline> must be a numeric vector.")
    }
    if (length(baseline) != 1 && length(baseline) != length(peaks)) {
      rlang::abort("<baseline> must be either length 1 or the same length as <peaks>.")
    }
  }
}
