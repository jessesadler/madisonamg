## Test reports ##

tbl <- ex_trace_tbl
x <- find_peaks_manual(
  start = c(35390, 45451, 55504, 65557, 75576, 85656, 95718, 105768, 115828, 125888),
  end   = c(35593, 45650, 55700, 65771, 75825, 85877, 95932, 105978, 116042, 126087))
report <- create_report(tbl, x)

test_that("creating reports works", {
  # The basics work
  expect_equal(class(report), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(report),
               c("pot_no", "peak_amp", "amp_decr_pct", "area", "area_decr_pct", "delay_mS"))
  expect_equal(nrow(report), 10)
})

test_that("the variables are correctly calculated", {
  expect_equal(report$pot_no, seq_along(x))
  # peak amp
  expect_true(all(report$peak_amp > 2800))
  expect_true(all(report$peak_amp[2:10] < report$peak_amp[[1]]))
  # amp_decr_pct
  expect_equal(report$amp_decr_pct[[1]], 0)
  expect_true(all(report$amp_decr_pct >= 0))
  # area
  expect_true(all(report$area > 283450))
  expect_equal(report$area_decr_pct > 0,
               c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  # delay
  expect_equal(report$delay_mS, peaks_delay(tbl, x))
})

test_that("baseline has an effect", {
  base0 <- create_report(tbl, x, baseline = 0)
  expect_true(all(base0$area > report$area))
  expect_false(identical(base0$area_decr_pct, report$area_decr_pct))

  base400 <- create_report(tbl, x, baseline = 400)
  expect_true(all(base400$area < report$area))
  expect_false(identical(base400$area_decr_pct, report$area_decr_pct))
})
