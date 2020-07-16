## Test methods for finding peaks ##

tbl <- ex_trace_tbl

test_that("find peaks by response works", {
  # That it works with good defaults
  expect_true(is.list(find_peaks_response(tbl)))
  expect_equal(length(find_peaks_response(tbl)), 10)

  # That stage 1 of function works
  # Lowering min_amp to find more peaks
  expect_gt(length(find_peaks_response(tbl, min_amp = 500)), 10)
  # Lowering min_length should find even more
  expect_gt(length(find_peaks_response(tbl, min_amp = 500, min_length = 20)),
              length(find_peaks_response(tbl, min_amp = 500)))

  # That stage 2 of function works
  # Lowering lengthen makes peaks shorter and values should be greater than min_amp
  expect_gt(length(find_peaks_response(tbl)[[1]]),
            length(find_peaks_response(tbl, lengthen = 0)[[1]]))
  expect_gt(min(find_values(tbl, find_peaks_response(tbl, lengthen = 0)[[1]])),
            1500)
  # Baseline correctly shortens peaks even with long lenthen
  expect_equal(length(find_peaks_response(tbl, lengthen = 100)[[1]]),
               length(find_peaks_response(tbl, lengthen = 10000)[[1]]))
  # Opposite is also true
  expect_equal(length(find_peaks_response(tbl, lengthen = 0)[[1]]) + 100,
               length(find_peaks_response(tbl, baseline = 0)[[1]]))
})

test_that("find peaks by stimulus works", {
  # That it works with good defaults
  expect_true(is.list(find_peaks_stimulus(tbl, delay = 5)))
  expect_equal(length(find_peaks_stimulus(tbl, delay = 5)), 10)

  # Changing delay changes length of peaks
  expect_gt(length(find_peaks_stimulus(tbl, delay = 3)[[1]]),
            length(find_peaks_stimulus(tbl, delay = 5)[[1]]))

  # Baseline changes length of peaks
  expect_gt(length(find_peaks_stimulus(tbl, delay = 5, baseline = NULL)[[1]]),
            length(find_peaks_stimulus(tbl, delay = 5, baseline = 400)[[1]]))
  expect_gt(length(find_peaks_stimulus(tbl, delay = 5, baseline = 0)[[1]]),
            length(find_peaks_stimulus(tbl, delay = 5, baseline = NULL)[[1]]))

  # min_length works
  expect_equal(length(find_peaks_stimulus(tbl, delay = 10)[[1]]), 100)

})

test_that("find peaks manually works", {
  # That is works
  expect_true(is.numeric(find_peaks_manual(100, 200)))
  expect_true(is.list(find_peaks_manual(c(100, 300), c(199, 399))))

  expect_equal(length(find_peaks_manual(100, 200)), 101)
  expect_equal(length(find_peaks_manual(c(100, 300), c(199, 399))), 2)

  # errors
  expect_error(find_peaks_manual("a", "b"),
               "<start> and <end> must be numeric vectors.")
  expect_error(find_peaks_manual(3, c(1, 2, 3)),
               "<start> and <end> must be the same length.")
})
