% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{full_plot}
\alias{full_plot}
\alias{viz_response}
\alias{viz_highlight_peaks}
\title{Visualize full response of trace}
\usage{
viz_response(
  df,
  buffer = 100,
  stimulus_diff = 9000,
  freq = 10000,
  title = NULL,
  show_stimulus = TRUE,
  filter_response = TRUE
)

viz_highlight_peaks(
  df,
  peaks,
  buffer = 100,
  stimulus_diff = 9000,
  freq = 10000,
  title = NULL,
  show_stimulus = TRUE,
  filter_response = TRUE,
  highlight_color = "dodgerblue"
)
}
\arguments{
\item{df}{Data frame or tibble with sample, stimulus, and response columns.}

\item{buffer}{Buffer to add to the beginning and end of the plot in
milliseconds if \code{filter_response = TRUE}.}

\item{stimulus_diff}{The stimulus is found by seeing when the amplitude of
the stimulus recording increases by a given amount from the previous
sample. Default is 9000, which seems to work for most recordings.}

\item{freq}{Frequency of recording.}

\item{title}{Title for the plot. A character vector of length 1.}

\item{show_stimulus}{Default \code{TRUE}. Show onset of stimuli or not.}

\item{filter_response}{Whether to only visualize the response from the
first to the last stimulus (\code{TRUE}) or to visualize the whole recording
(\code{FALSE}). Used to exclude extraneous recording. Default is \code{TRUE}.}

\item{peaks}{List of numeric vectors with the samples for where the peaks
occur.}

\item{highlight_color}{Color to be used to highlight response peaks.}
}
\value{
A ggplot2 object.
}
\description{
Visualize full response of a trace. The trace can be filtered from the first
stimulus to the last stimulus to better visualize peaks or not, which may be
useful to find any oddities in the data. Use \code{viz_highlight_peaks()} to
highlight peaks.
}
\examples{
viz_response(ex_trace_tbl)

# Highlight response peaks
x <- find_peaks_response(ex_trace_tbl)
viz_highlight_peaks(ex_trace_tbl, x)

# Show complete recorded trace
viz_response(ex_trace_tbl, filter_response = FALSE)

}
\seealso{
See \code{viz_response_interactive()} for an interactive visualization
of the response. See \code{viz_stimulus()} for plot of the stimulus.
}
