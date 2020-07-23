## Test methods for finding stimuli ##

tbl <- ex_trace_tbl

test_that("internal stimuli filter function works", {
  # It works
  expect_equal(nrow(stimuli_filter(tbl, 9000)), 10)
  expect_equal(names(stimuli_filter(tbl, 9000)),
               c("sample", "secs", "stimulus", "response", "amp_diff"))

  # stimulus_diff works
  expect_gt(nrow(stimuli_filter(tbl, 500)), 10)
  expect_lt(nrow(stimuli_filter(tbl, 30000)), 10)
})

test_that("internal stimuli samples works", {
  expect_true(is.integer(stimuli_samples(tbl, 9000)))
  expect_equal(stimuli_samples(tbl, 9000),
               c(35331, 45385, 55439, 65492, 75545, 85598, 95653,
                 105709, 115765, 125821))
})

test_that("find stimuli function works", {
  expect_equal(nrow(find_stimuli(tbl, 9000)), 10)
  expect_equal(names(find_stimuli(tbl, 9000)),
               c("sample", "secs", "stimulus", "response",
                 "amp_diff", "sample_diff", "sec_diff", "hz"))
  expect_equal(find_stimuli(tbl, 9000)$hz, c(NA, rep(1, 9)))
})
