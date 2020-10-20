# waveMC_to_tbl -----------------------------------------------------------

#' Cast waveMC to tibble
#'
#' Cast a waveMC object created by `tuneR::readWave()` to a tibble.
#'
#' @details This function is based on a waveMC object with left and right
#' channels. The channel with the highest recorded value is made into the
#' stimulus variable. An error is thrown if `wav` is a wave object with mono
#' sound instead of a waveMC object.
#'
#' @param wav waveMC object with left and right channels.
#' @param freq Frequency of recording. This is used to create the `secs`
#'   column.
#'
#' @return Returns a tibble with four columns:
#'
#' - `sample`: integer representing each sample from the data.
#' - `secs`: time value of each sample in seconds.
#' - `stimulus`: integer reading of the stimulus recording.
#' - `response`: integer reading of the response recording.
#'
#' All other functions in `madisonamg` are built around a tibble with this
#' structure.
#'
#' @examples
#' waveMC_to_tbl(ex_waveMC, freq = 10000)
#'
#' @export

waveMC_to_tbl <- function(wav, freq) {
  # Check that wav is waveMC
  if (class(wav)[[1]] != "WaveMC") {
    rlang::abort("<wav> must be an object of class <WaveMC>.")
  }

  # Access data from waveMC object
  left <- wav@.Data[, 1] # left channel
  right <- wav@.Data[, 2] # right channel

  # Determine which channel is stimulus and which is response

  if (identical(max(left), max(right))) {
    c1 <- "stimulus"
    c2 <- "response"

    rlang::warn("Maximum amplitude of the stimulus and response are identical.",
                " Check the data and also confirm which channel is the response",
                " and which is the stimulus.")
  } else {
    c1 <- dplyr::if_else(max(left) > max(right), "stimulus", "response")
    c2 <- dplyr::if_else(max(left) < max(right), "stimulus", "response")
  }

  # Create and return tibble
  tibble::tibble(!!c1 := left,
                 !!c2 := right,
                 sample = seq_along(left)) %>%
    dplyr::mutate(secs = sample / freq) %>%
    dplyr::select(sample, .data$secs, !!c1, !!c2)
}
