## Data documentation ##

#' Example waveMC data
#'
#' waveMC object with 5 channels, but only two with data. It has a sampling
#' rate of 10,000 and is 19.28 seconds long, and contains 192,804 samples.
#'
#' @format A waveMC object.
"ex_waveMC"

#' Example trace tibble data
#'
#' A tibble derived from `waveMC_ex` showing the entire trace of the recording.
#'
#' @format A tibble with 192,804 rows and 4 variables.
#'
#' @section Variables:
#'
#'   - `sample`: Sample number for the trace.
#'   - `secs`: Number of seconds elapsed.
#'   - `stimulus`: Reading for the stimulus.
#'   - `response`: Reading for the response.
"ex_trace_tbl"
