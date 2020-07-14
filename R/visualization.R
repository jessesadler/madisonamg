# Visualization -----------------------------------------------------------

#' Visualize full response of trace
#'
#' Visualize full response of a trace. The trace can be filtered from the first
#' stimulus to the last stimulus to better visualize peaks or not, which may be
#' useful to find any oddities in the data. Use `viz_highlight_peaks()` to
#' highlight peaks.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_checks
#' @param freq Frequency of recording.
#' @param buffer Buffer to add to the beginning and end of the plot in
#'   milliseconds if `filter_response = TRUE`.
#' @param title Title for the plot
#' @param show_stimulus Default `TRUE`. Show onset of stimuli or not.
#' @param filter_response Whether to filter response from the first to the
#'   last stimulus. Default is `TRUE`.
#' @param highlight_color Color to be used to highlight response peaks.
#'
#' @seealso See `viz_response_interactive()` for an interactive visualization
#'   of the response. See `viz_stimulus()` for plot of the stimulus.
#'
#' @examples
#' viz_response(ex_trace_tbl)
#'
#' # Highlight response peaks
#' x <- find_peaks_response(ex_trace_tbl)
#' viz_highlight_peaks(ex_trace_tbl, x)
#'
#' # Show complete recorded trace
#' viz_response(ex_trace_tbl, filter_response = FALSE)
#'
#' @return A ggplot2 object.
#'
#' @name full_plot
NULL

#' @rdname full_plot
#' @export
viz_response <- function(df, buffer = 100,
                         stimulus_diff = 9000, freq = 10000,
                         title = NULL,
                         show_stimulus = TRUE,
                         filter_response = TRUE) {

  stimuli_df <- stimuli_filter(df, stimulus_diff)

  if (filter_response) {
    df <- buffer_df(df, stimuli_df$sample, buffer, freq)
  }

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = .data$sample, y = .data$response)) +
    ggplot2::labs(title = glue::glue({title}),
                  x = NULL, y = NULL)

  if (show_stimulus) {
    p <- p +
      ggplot2::geom_point(data = stimuli_df,
                          ggplot2::aes(x = .data$sample, y = .data$response),
                          color = "red")
  }
  p
}

#' @rdname full_plot
#' @export
viz_highlight_peaks <- function(df, peaks, buffer = 100,
                                stimulus_diff = 9000, freq = 10000,
                                title = NULL,
                                show_stimulus = TRUE,
                                filter_response = TRUE,
                                highlight_color = "dodgerblue") {

  p <- viz_response(df = df, buffer = buffer,
                         stimulus_diff = stimulus_diff, freq = freq,
                         title = title,
                         show_stimulus = show_stimulus,
                         filter_response = filter_response)
  # peaks_tbl for highlight
  peaks_tbl <- filter_peaks(df, peaks)

  p +
    ggplot2::geom_line(data = peaks_tbl,
                       ggplot2::aes(x = .data$sample,
                                    y = .data$response,
                                    group = .data$peak_nr),
                       color = highlight_color)

}

#' Visualize full stimulus of trace
#'
#' Visualize full stimulus of a trace. The trace can be filtered from the first
#' stimulus to the last stimulus to better visualize peaks or not, which may be
#' useful to find any oddities in the data.
#'
#' This plot will not be of use for actual analysis. Instead, it is meant to
#' provide a quick and easy way to see if there are any oddities in the
#' stimulus data.
#'
#' @inheritParams find_stimuli
#' @inheritParams full_plot
#' @param filter_stimulus Whether to filter stimulus from the first to the
#'   last stimulus. Default is `TRUE`.
#'
#' @examples
#'
#' viz_stimulus(ex_trace_tbl)
#'
#' @export

viz_stimulus <- function(df, buffer = 100,
                         stimulus_diff = 9000, freq = 10000,
                         title = NULL,
                         filter_stimulus = TRUE) {

  if (filter_stimulus) {
    stimuli <- stimuli_samples(df, stimulus_diff)
    df <- buffer_df(df, stimuli, buffer, freq)
  }

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = .data$sample, y = .data$stimulus)) +
    ggplot2::labs(title = glue::glue({title}),
                  x = NULL, y = NULL)

  p
}

#' Facet wrap visualization of response peaks
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_checks
#'
#' @examples
#'
#' x <- find_peaks_response(ex_trace_tbl)
#' viz_peak_facet(ex_trace_tbl, x)
#'
#' @return A ggplot2 object.
#'
#' @export

viz_peak_facet <- function(df, peaks) {
  peak_nr <- seq(length(peaks))

  purrr::map2_df(.x = peaks, .y = peak_nr,
                 ~ df %>%
                   dplyr::filter(.data$sample %in% .x) %>%
                   dplyr::mutate(peak = .y)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = .data$sample, y = .data$response)) +
    ggplot2::facet_wrap(~ peak, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL)
}


#' Visualize response peak
#'
#' Visualization of response peak with options to show peak area,
#' show stimulus, and annotate.
#'
#' @inheritParams find_stimuli
#' @inheritParams peaks_checks
#' @inheritParams full_plot
#' @param buffer Buffer to add to the beginning and end of the plot in
#'   milliseconds.
#' @param peak_nr Number of the peak that you want to visualize.
#' @param baseline If `NULL` (the default), the baseline for calculating
#'   area under the curve and highlighting the peak area is the minimum
#'   response value for the peak. Otherwise, baseline can be provided.
#' @param show_peak_area Whether to highlight the area of the peak.
#'   Default is TRUE.
#' @param annotate Whether to show annotation of the area under the curve and
#'   the delay from the onset of the stimulus to the start of the peak. Default
#'   is TRUE.
#' @param min_delay Minimum delay between onset of stimulus and start of
#'   response peak in milliseconds. If smaller, a warning is produced.
#' @param max_delay Maximum delay between onset of stimulus and start of
#'   response peak in milliseconds. If larger, a warning is produced.
#'
#' @return A ggplot2 object.
#'
#' @examples
#'
#' x <- find_peaks_response(ex_trace_tbl)
#' viz_peak(ex_trace_tbl, x, peak_nr = 5)
#'
#' # Change baseline
#' viz_peak(ex_trace_tbl, x, peak_nr = 5, baseline = 0)
#'
#' # Change color
#' viz_peak(ex_trace_tbl, x, peak_nr = 5, highlight_color = "orange")
#'
#' # Remove annotations and stimulus
#' viz_peak(ex_trace_tbl, x, peak_nr = 5,
#'          show_stimulus = FALSE,
#'          annotate = TRUE)
#'
#' @export

viz_peak <- function(df, peaks, peak_nr, buffer = 10,
                     stimulus_diff = 9000, freq = 10000,
                     show_peak_area = TRUE,
                     highlight_color = "dodgerblue",
                     baseline = NULL,
                     show_stimulus = TRUE,
                     annotate = TRUE,
                     min_delay = 0, max_delay = 50) {

  peak_samples <- peaks[[peak_nr]]
  peak_df <- dplyr::filter(df, .data$sample %in% peak_samples)

  # Objects in multiple if statements
  if (is.null(baseline)) {
    baseline <- min(peak_df$response)
  }
  auc <- pracma::trapz(peak_df$sample, peak_df$response - baseline)
  stimulus <- stimuli_samples(df, stimulus_diff)[[peak_nr]]
  delay <- samples_to_ms(min(peak_samples) - stimulus, freq)

  # Create data frame with buffer
  buffer_df <- buffer_df(df, extent = peak_samples, buffer, freq)

  # Base plot
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$sample, y = .data$response)) +
    ggplot2::geom_line(data = buffer_df) +
    ggplot2::geom_line(data = peak_df, color = highlight_color) +
    ggplot2::labs(title = glue::glue("Peak: {peak_nr}"),
                  x = NULL,
                  y = NULL)

  if (show_stimulus) {
    # Less than min delay
    if (delay < min_delay) {
      warning(call. = FALSE,
              glue::glue("Peak begins less than {min_delay} milliseconds",
                         " after the stimulus."))
    }

    # More than max delay
    if (delay > max_delay) {
      warning(call. = FALSE,
              glue::glue("Peak begins more than {max_delay} milliseconds",
                         " after the stimulus."))
    }
    p <- p + ggplot2::geom_vline(xintercept = stimulus, color = "red")
  }

  if (show_peak_area) {
    p <- p +
      ggplot2::geom_ribbon(data = peak_df,
                           ggplot2::aes(ymin = baseline, ymax = .data$response),
                           fill = highlight_color, alpha = 0.8)
  }

  if (annotate) {
    p <- p +
      ggplot2::annotate("text",
                        x = stimulus + ms_to_samples(delay, freq) / 2,
                        y = mean(peak_df$response),
                        label = glue::glue("auc:
                                           {scales::comma(auc)}

                                           Delay of
                                           {delay} mS"))
  }
  p
}


# dygraph -----------------------------------------------------------------

# See https://community.rstudio.com/t/create-pipeline-of-function-calls-from-list/6511
# for discussion of creating pipeline in function.

#' Interactive full response trace visualization
#'
#' Interactive visualization of a trace. The trace can be filtered from the
#' first stimulus to the last stimulus to better visualize peaks. Interactive
#' plot enables the user to zoom in on peaks. There are also options to show
#' onset of stimuli and shade the area of the peaks.
#'
#' @inheritParams find_stimuli
#' @inheritParams full_plot
#' @param peaks List of numeric vectors with the samples for where the peaks
#'   occur. Default is `NULL`, which makes `shade_peaks = FALSE`.
#' @param shade_peaks Whether to shade/highlight the area of the peaks.
#'   Default is TRUE.
#'
#' @seealso `viz_response()` and `viz_highlight_peaks()` for ggplot2 plots.
#'
#' @examples
#' viz_response_interactive(ex_trace_tbl)
#'
#' # Shade peaks
#' x <- find_peaks_response(ex_trace_tbl)
#' viz_response_interactive(ex_trace_tbl, x)
#'
#' @export

viz_response_interactive <- function(df, peaks = NULL,
                                     buffer = 100, stimulus_diff = 9000,
                                     freq = 10000,
                                     filter_response = TRUE,
                                     show_stimulus = TRUE,
                                     shade_peaks = TRUE) {
  if (is.null(peaks)) {
    shade_peaks <- FALSE
  }

  # vector of stimuli
  stimuli <- stimuli_samples(df, stimulus_diff)

  if (filter_response) {
    df <- buffer_df(df, stimuli, buffer, freq)
  }

  # Prep df for plotting with dygraphs
  response <- dplyr::select(df, sample, response)

  # build dygraph
  p <- dygraphs::dygraph(response) %>%
    dygraphs::dyRangeSelector()

  if (show_stimulus) {
    # build list of args
    stim_list <- purrr::map(seq_along(stimuli),
                            ~ list(fct = rlang::expr(dygraphs::dyEvent),
                                   args = list(stimuli[[.x]])))
    # build pipeline
    events <- purrr::reduce(stim_list,
                            ~ rlang::expr(!!.x %>% !!rlang::call2(.y$fct, !!!.y$args)),
                            .init = p)
    # add to plot
    p <- rlang::eval_tidy(events)
  }

  if (shade_peaks) {
    # build list of args
    res_list <- purrr::map(seq_along(peaks),
                           ~ list(fct = rlang::expr(dygraphs::dyShading),
                                  args = list(from = min(peaks[[.x]]),
                                              to = max(peaks[[.x]]))))
    # build pipeline
    shading <- purrr::reduce(res_list,
                             ~ rlang::expr(!!.x %>% !!rlang::call2(.y$fct, !!!.y$args)),
                             .init = p)
    # add to plot
    p <- rlang::eval_tidy(shading)
  }
  # plot
  p
}
