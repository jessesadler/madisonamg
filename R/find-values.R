#' Find response amplitude values at given samples
#'
#' @inheritParams find_stimuli
#' @param samples Numeric vector or list of vectors of sample numbers.
#'   Usually this will be a list of samples of peaks from
#'   `find_peaks_response()` or `find_peaks_stimulus()`.
#'
#' @return Returns either a list or a vector of response values depending
#'   on whether `samples` is a list or vector.
#'
#' @examples
#' peaks <- find_peaks_response(ex_trace_tbl)
#'
#' find_values(ex_trace_tbl, samples = peaks)
#'
#' @export

find_values <- function(df, samples) {
  # If statement for whether samples is a vector or list

  if (is.list(samples)) {
    # Ensure samples is list of numeric vectors
    if (!all(purrr::map_lgl(samples, is.numeric))) {
      stop(call. = FALSE,
           "<samples> must be a numeric vector or list of numeric vectors.")
    }
    # Create vector of samples and filter df by smpl_vctr
    smpl_vctr <- purrr::flatten_int(samples)

    out_df <- dplyr::filter(df, .data$sample %in% smpl_vctr)

    # Split vector to create a list by finding where samples are discontinuous
    split(out_df$response, cumsum(c(TRUE, diff(out_df$sample) != 1L))) %>%
      purrr::set_names(NULL)

  } else if (is.numeric(samples)) {
    # Filter samples and get response vector
    dplyr::filter(df, .data$sample %in% samples) %>%
      dplyr::pull(.data$response)
  } else {
    stop(call. = FALSE,
         "<samples> must be a numeric vector or list of numeric vectors.")
  }
}
