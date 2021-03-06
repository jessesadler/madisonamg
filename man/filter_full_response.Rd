% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter-tbl.R
\name{filter_full_response}
\alias{filter_full_response}
\title{Filter trace data frame by response}
\usage{
filter_full_response(df, peaks, buffer = 100, freq = 10000)
}
\arguments{
\item{df}{Data frame or tibble with sample, stimulus, and response columns.}

\item{peaks}{List of numeric vectors with the samples for where the peaks
occur.}

\item{buffer}{Default is 100. Milliseconds to add before first stimulus
and after the last stimulus.}

\item{freq}{Frequency of recording. Used to convert milliseconds to samples.}
}
\description{
Filter out anything before the first peak and after the last peak.
This gets rid of all recordings before and after the performance of the
experiment. This is particularly useful for plotting the full trace.
}
\examples{
x <- find_peaks_response(ex_trace_tbl)

filter_full_response(ex_trace_tbl, x)

}
\seealso{
\code{filter_full_stimuli()}
}
