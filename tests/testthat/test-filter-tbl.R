## Test filter trace tibble ##

tbl <- ex_trace_tbl
x <- find_peaks_manual(
  start = c(35390, 45451, 55504, 65557, 75576, 85656, 95718, 105768, 115828, 125888),
  end   = c(35593, 45650, 55700, 65771, 75825, 85877, 95932, 105978, 116042, 126087))

peaks_sum <- sum(purrr::map_int(x, length))
peaks_extent <- length(seq(purrr::map_int(x, min)[[1]],
                           purrr::map_int(x, max)[[10]]))
stims <- stimuli_samples(tbl, 9000)
stimuli_extent <- length(seq(min(stims), max(stims)))

test_that("test filter_peaks", {
  # Correct column names
  expect_equal(names(filter_peaks(tbl, x)),
               c("sample", "secs", "stimulus", "response", "sub_sample", "peak_nr"))
  # Correct values in peak_nr and sub_sample columns
  expect_equal(unique(filter_peaks(tbl, x)$peak_nr), 1:10)
  expect_equal(unique(filter_peaks(tbl, x)$sub_sample),
               1:max(purrr::map_int(x, length)))
  # Correct filtering
  expect_equal(nrow(filter_peaks(tbl, x)), peaks_sum)

  # With length_out: Correct filtering with more than max and less than min
  expect_equal(nrow(filter_peaks(tbl, x, length_out = 300)), 3000)
  expect_equal(nrow(filter_peaks(tbl, x, length_out = 100)), 1000)
})

test_that("test filter by stimuli", {
  expect_equal(nrow(filter_full_stimuli(tbl, buffer = 0)), stimuli_extent)
  expect_equal(nrow(filter_full_stimuli(tbl)), stimuli_extent + 2000)
  expect_equal(nrow(filter_full_stimuli(tbl, buffer = 50)), stimuli_extent + 1000)
})

test_that("test filter by response", {
  expect_equal(nrow(filter_full_response(tbl, x, buffer = 0)), peaks_extent)
  expect_equal(nrow(filter_full_response(tbl, x)), peaks_extent + 2000)
  expect_equal(nrow(filter_full_response(tbl, x, buffer = 50)), peaks_extent + 1000)
})
