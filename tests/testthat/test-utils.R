## Test utils ##

tbl <- ex_trace_tbl

sub_samples <- c(3000:3999, 5000:5999, 7000:7999, 9000:9999)
sub_response <- tbl$response[sub_samples]

test_that("split list works", {
  expect_true(is.list(split_list(sub_response, sub_samples)))
  expect_equal(length(split_list(sub_response, sub_samples)), 4L)
  # split_list is the opposite of unlist
  expect_equal(unlist(split_list(sub_response, sub_samples)), sub_response)
})

test_that("ms conversion works", {
  expect_equal(samples_to_ms(1, 1), 1000)
  expect_equal(samples_to_ms(1, 10000), 0.1)

  expect_equal(ms_to_samples(1, 1), 0.001)
  expect_equal(ms_to_samples(1, 10000), 10)
})

test_that("buffer_df works", {
  expect_equal(nrow(buffer_df(tbl, sub_samples, 0, 10000)),
               max(sub_samples) - min(sub_samples) + 1)

  expect_equal(nrow(buffer_df(tbl, sub_samples, 50, 10000)),
               max(sub_samples) - min(sub_samples) + 1001)
})
