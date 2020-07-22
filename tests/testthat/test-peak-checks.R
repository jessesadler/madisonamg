## Test checks for peaks ##

tbl <- ex_trace_tbl
x <- find_peaks_manual(
  start = c(35390, 45451, 55504, 65557, 75576, 85656, 95718, 105768, 115828, 125888),
  end   = c(35593, 45650, 55700, 65771, 75825, 85877, 95932, 105978, 116042, 126087))
y <- find_peaks_stimulus(tbl, delay = 5)

test_that("checks for peak groups work", {
  expect_equal(peaks_min_amp(tbl, x), rep(320, 10))
  expect_equal(peaks_min_amp(tbl, 2000:2050), -128)

  expect_equal(peaks_max_amp(tbl, x),
               c(3584, 2816, 2880, 3008, 3328, 3008, 3008, 3136, 3072, 3392))
  expect_equal(peaks_max_amp(tbl, 2000:2050), 64)

  expect_equal(peaks_first_amp(tbl, x),
               c(320, 320, 320, 320, 384, 320, 320, 320, 320, 320))
  expect_equal(peaks_first_amp(tbl, 2000:2050), -128)

  expect_equal(peaks_last_amp(tbl, x),
               c(320, 448, 448, 448, 320, 320, 384, 320, 448, 320))
  expect_equal(peaks_last_amp(tbl, 2000:2050), 64)
})

test_that("peaks delay works", {
  expect_equal(peaks_delay(tbl, y), rep(5, 10))
  expect_warning(peaks_delay(tbl, x, min_delay = 6),
                 "Peaks 1, 5, 6 and 8 begin less than 6 milliseconds after the stimulus.")
  expect_warning(peaks_delay(tbl, x, max_delay = 6.5),
                 "Peaks 2 and 10 begin more than 6.5 milliseconds after the stimulus.")
})
