tbl <- ex_trace_tbl
x <- find_peaks_manual(
  start = c(35390, 45451, 55504, 65557, 75576, 85656, 95718, 105768, 115828, 125888),
  end   = c(35593, 45650, 55700, 65771, 75825, 85877, 95932, 105978, 116042, 126087))
lengths <- c(204, 200, 197, 215, 250, 222, 215, 211, 215, 200)

test_that("peaks length works", {
  expect_equal(peaks_length(x), lengths)
  expect_equal(peaks_length(x[[2]]), lengths[[2]])

  # Milliseconds
  expect_equal(peaks_length(x, samples = FALSE, freq = 10000), lengths / 10)
  expect_error(peaks_length(x, samples = FALSE),
               "<freq> must be provided if <samples> is FALSE.")
})

test_that("peaks auc works", {
  # Same as in report
  expect_equal(peaks_auc(tbl, x), create_report(tbl, x)$area)
  expect_equal(peaks_auc(tbl, x, baseline = 0),
               create_report(tbl, x, baseline = 0)$area)
  expect_equal(peaks_auc(tbl, x, baseline = 1:10),
               create_report(tbl, x, baseline = 1:10)$area)

  # Right answers
  expect_equal(peaks_auc(tbl, x),
               c(325952, 283456, 284608, 321984, 359712, 330816, 328160, 322048, 338688, 327680))
  expect_true(all(peaks_auc(tbl, x, baseline = 0) > peaks_auc(tbl, x)))
})
