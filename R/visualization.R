# Visualization -----------------------------------------------------------

viz_full_response <- function(df, buffer = 100,
                              stimulus_diff = 9000, freq = 10000,
                              title = NULL,
                              show_stimulus = TRUE) {
  stimuli_df <- df %>%
    find_stimuli(stimulus_diff = stimulus_diff)

  start <- min(stimuli_df$sample) - buffer * freq / 1000
  end <- max(stimuli_df$sample) + buffer * freq / 1000

  p <- df %>%
    dplyr::filter(sample >= start & sample <= end) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = .data$sample, y = .data$response)) +
    ggplot2::labs(title = glue::glue({title}),
                  x = NULL, y = NULL)

  if (show_stimulus) {
    p +
      ggplot2::geom_point(data = stimuli_df,
                          ggplot2::aes(x = .data$sample, y = .data$response),
                          color = "red")
  } else {
    p
  }

}

viz_highlight_peaks <- function(df, peaks, buffer = 100,
                                stimulus_diff = 9000, freq = 10000,
                                show_stimulus = TRUE) {
  samples <- purrr::flatten_int(peaks)
  vals <- find_values(df, peaks) %>%
    purrr::flatten_int()

  peak_lengths <- peaks_length(peaks)

  peak_nr <- purrr::map2(seq_along(peaks), peak_lengths,
                         ~ rep_len(.x, .y)) %>%
    purrr::flatten_int()

  response <- tibble::tibble(sample = samples,
                             response = vals,
                             group = peak_nr)

  p <- viz_full_response(df = df, buffer = buffer,
                         stimulus_diff = stimulus_diff, freq = freq,
                         show_stimulus = show_stimulus)

  p +
    ggplot2::geom_line(data = response,
                       ggplot2::aes(x = .data$sample,
                                    y = .data$response,
                                    group = .data$group),
                       color = "dodgerblue")

}

viz_peak <- function(df, peaks, peak,
                     buffer = 10, freq = 10000,
                     show_peak_area = TRUE,
                     baseline = NULL) {

  peak_samples <- peaks[[peak]]

  peak_df <- df %>%
    dplyr::filter(.data$sample %in% peak_samples)

  start <- min(peak_samples) - buffer * freq / 1000
  end <- max(peak_samples) + buffer * freq / 1000

  buffer_df <- df %>%
    dplyr::filter(.data$sample >= start & sample <= end)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$sample, y = .data$response)) +
    ggplot2::geom_line(data = buffer_df) +
    ggplot2::geom_line(data = peak_df, color = "dodgerblue") +
    ggplot2::labs(title = glue::glue("Peak: {peak}")) +
    ggplot2::labs(x = NULL, y = NULL)

  if (show_peak_area) {
    if (is.null(baseline)) {
      baseline <- min(peak_df$response)
    }

    p <- p +
      ggplot2::geom_ribbon(data = peak_df,
                           ggplot2::aes(ymin = .data$baseline, ymax = .data$response),
                           fill = "dodgerblue", alpha = 0.8)
  }
  p
}

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

viz_peak_delay <- function(df, peaks, peak, buffer = 10,
                           stimulus_diff = 9000, freq = 10000,
                           min_delay = 0, max_delay = 200,
                           show_peak_area = TRUE,
                           baseline = NULL) {
  peak_samples <- peaks[[peak]]

  stimuli <- df %>%
    find_stimuli(stimulus_diff = stimulus_diff) %>%
    dplyr::pull(.data$sample)
  stimulus <- stimuli[[peak]]

  peak_df <- dplyr::filter(df, .data$sample %in% peak_samples)

  start <- min(peak_samples) - buffer * freq / 1000
  end <- max(peak_samples) + buffer * freq / 1000

  buffer_df <- dplyr::filter(df, .data$sample >= start & sample <= end)

  delay <- min(peak_samples) - stimulus

  # Less than min delay
  if (delay < min_delay) {
    warning(call. = FALSE,
            glue::glue("Peak begins less than {min_delay} samples after the stimulus."))
  }

  # More than max delay
  if (delay > max_delay) {
    warning(call. = FALSE,
            glue::glue("Peak begins more than {max_delay} samples after the stimulus."))
  }

  p <- ggplot2::ggplot(ggplot2::aes(x = .data$sample, y = .data$response)) +
    ggplot2::geom_line(data = buffer_df) +
    ggplot2::geom_line(data = peak_df, color = "dodgerblue") +
    ggplot2::geom_vline(xintercept = stimulus, color = "red") +
    ggplot2::annotate("text",
                      x = stimulus + delay / 2,
                      y = mean(peak_df$response) / 2,
                      label = glue::glue("Delay of
                                         {delay * 1000 / freq} mS")) +
    ggplot2::labs(title = glue::glue("Peak: {peak}"),
                  x = NULL,
                  y = NULL)

  if (show_peak_area) {
    if (is.null(baseline)) {
      baseline <- min(peak_df$response)
    }

    auc <- pracma::trapz(peak_df$sample, peak_df$response - baseline)

    p <- p +
      ggplot2::geom_ribbon(data = peak_df,
                           ggplot2::aes(ymin = .data$baseline, ymax = .data$response),
                           fill = "dodgerblue", alpha = 0.8) +
      ggplot2::annotate("text",
                        x = stimulus + delay / 2,
                        y = mean(peak_df$response),
                        label = glue::glue("auc:
                                           {scales::comma(auc)}"))
  }
  p
}


# dygraph -----------------------------------------------------------------

# See https://community.rstudio.com/t/create-pipeline-of-function-calls-from-list/6511
# for discussion of creating pipeline in function.

interactive_trace <- function(df, peaks = NULL,
                              buffer = 100, stimulus_diff = 9000,
                              show_stimuli = TRUE,
                              show_response_zone = TRUE) {
  if (is.null(peaks)) {
    show_response_zone <- FALSE
  }
  # Prep df for plotting with dygraphs
  response <- dplyr::select(df, sample, response)

  # build dygraph
  p <- dygraphs::dygraph(response) %>%
    dygraphs::dyRangeSelector()

  if (isTRUE(show_stimuli)) {
    # vector of stimuli
    stimuli <- find_stimuli(df, stimulus_diff = stimulus_diff) %>%
      dplyr::pull(sample)

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

  if (isTRUE(show_response_zone)) {
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
