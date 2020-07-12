#' Find values at given samples
#'
#' @inheritParams find_stimuli
#' @param samples Numeric vector or list of vectors of sample numbers.
#'   Usually this will be a list of samples of peaks.
#'
#' @return Returns either a list or a vector of response values depending
#'   on whether `samples` is a list or vector.
#'
#' @export

find_values <- function(df, samples) {
  if (is.list(samples)) {
    smpl_vctr <- purrr::flatten_int(samples)

    out_df <- dplyr::filter(df, sample %in% smpl_vctr)

    split(out_df$response, cumsum(c(TRUE, diff(out_df$sample) != 1L))) %>%
      purrr::set_names(NULL)
  } else if (is.numeric(samples)) {
    smpl_vctr <- samples

    dplyr::filter(df, sample %in% smpl_vctr) %>%
      dplyr::pull(response)
  } else {
    stop(call. = FALSE,
         "<samples> must be a numeric vector or list of numeric vectors.")
  }
}
