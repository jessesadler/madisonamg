# create_reports ----------------------------------------------------------

#' Create AMG reports
#'
#' @inheritParams find_stimuli
#' @inheritParams find_peaks_stimulus
#' @param baseline If `NULL` (the default), the baseline for calculating
#'   area under the curve is the minimum response value for each peak.
#'   Otherwise, baseline can be a single numeric value for a consistent
#'   baseline, or a numeric vector of length equal to the number of peaks.
#'
#' @return Returns a report of the trace in the form of a tibble with
#'   six columns:
#'
#'   - `pot_no`: The peak number.
#'   - `peak_amp`: Maximum amplitude of the peak response.
#'   - `amp_decr_pct`: Percentage decrease of the maximum amplitude from
#'   the first peak.
#'   - `area`: Area under the curve of the response, taking into account the
#'   `baseline`.
#'   - `area_decr_pct`: Percentage decrease of the area under the curve from
#'   the first peak.
#'   - `delay_mS`: Delay between the stimulus and the onset of the response
#'   in milliseconds.
#'
#' @export

create_report <- function(df, peaks,
                          stimulus_diff = 9000,
                          freq = 10000,
                          baseline = NULL) {
  stimuli <- df %>%
    find_stimuli(stimulus_diff = stimulus_diff) %>%
    dplyr::pull(sample)

  if (length(stimuli) != length(peaks)) {
    stop(call. = FALSE,
         paste0("Number of stimuli found (", nrow(stimuli),
                ") is different from peaks (", peaks, ")."), "\n",
         "       Check that <peaks> is correct or alter <stimulus_diff>.")
  }
  # Collect pieces for report
  # Delay in milliseconds
  delay <- (purrr::map_dbl(peaks, min) - stimuli) * 1000 / freq
  values <- sub_values(peaks, df$response)
  peak_amp <- purrr::map_dbl(values, max)

  # 100 - pct to make value the decreasing percentage
  # Multiply by 100 to make value a percentage
  amp_decr_pct <- 100 - (peak_amp / peak_amp[[1]]) * 100

  # Area under the curve
  if (is.null(baseline)) {
    baseline <- purrr::map_int(values, min)
  }
  values <- map2(values, baseline, ~ .x - .y)

  auc <- purrr::map2_dbl(peaks, values, ~ pracma::trapz(.x, .y))
  area_decr_pct <- 100 - (auc / auc[[1]]) * 100

  # Create report
  tibble::tibble(pot_no = seq(length(peaks)),
                 peak_amp = peak_amp,
                 amp_decr_pct = amp_decr_pct,
                 area = auc,
                 area_decr_pct = area_decr_pct,
                 delay_mS = delay)
}
