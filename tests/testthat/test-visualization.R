## Tests for visualization functions ##

tbl <- ex_trace_tbl
x <- find_peaks_manual(
  start = c(35390, 45451, 55504, 65557, 75576, 85656, 95718, 105768, 115828, 125888),
  end   = c(35593, 45650, 55700, 65771, 75825, 85877, 95932, 105978, 116042, 126087))

test_that("plotting the response works", {
  vdiffr::expect_doppelganger("viz_response works",
                              viz_response(tbl))
  vdiffr::expect_doppelganger("viz_response buffer",
                              viz_response(tbl, buffer = 1000))
  vdiffr::expect_doppelganger("viz_response stimulus",
                              viz_response(tbl, show_stimulus = FALSE))
  vdiffr::expect_doppelganger("viz_response filter",
                              viz_response(tbl, filter_response = FALSE))
})

test_that("highlighting peaks works", {
  vdiffr::expect_doppelganger("highlight works",
                              viz_highlight_peaks(tbl, x))
  vdiffr::expect_doppelganger("highlight color works",
                              viz_highlight_peaks(tbl, x, highlight_color = "orange"))
})

test_that("plotting the stimulus works", {
  vdiffr::expect_doppelganger("stimulus plot works", viz_stimulus(tbl))
})

test_that("facet peaks works", {
  vdiffr::expect_doppelganger("peak facet plot works", viz_peak_facet(tbl, x))
})

test_that("plotting a peak works", {
  vdiffr::expect_doppelganger("peak plot works", viz_peak(tbl, x, 5))
  vdiffr::expect_doppelganger("peak plot arguments as false",
                              viz_peak(tbl, x, 4, show_peak_area = FALSE,
                                       show_stimulus = FALSE, annotate = FALSE))

  expect_warning(viz_peak(tbl, x, 5, min_delay = 5))
  expect_warning(viz_peak(tbl, x, 5, max_delay = 2))
})

test_that("dygraph plot works", {
  expect_s3_class(viz_response_interactive(tbl, x), c("dygrapgs", "htmlwidget"))
})
