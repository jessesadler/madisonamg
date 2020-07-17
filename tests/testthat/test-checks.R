## Tests for checks ##

tbl <- ex_trace_tbl
x <- find_peaks_manual(
  start = c(35390, 45451, 55504, 65557, 75576, 85656, 95718, 105768, 115828, 125888),
  end   = c(35593, 45650, 55700, 65771, 75825, 85877, 95932, 105978, 116042, 126087))
y <- stimuli_samples(tbl, 9000)

test_that("checks for trace tibble work", {
  # Good ones work
  expect_silent(check_trace(tbl))

  # df is a data.frame
  expect_error(check_trace(y),
               glue::glue("<df> must be a data frame with variables named `sample`,",
                          " `stimulus`, and `response`, all of which must be numeric."))
  # necessary variables are present
  expect_error(check_trace(tbl[ , 1:3]),
               "<df> must have variables named `sample`, `stimulus`, and `response`.")

  # Variables are numeric
  df1 <- data.frame(sample = letters[1:10],
                    stimulus = 1:10,
                    response = 1:10)
  expect_error(check_trace(df1), "`sample` must be a numeric vector.")

  df2 <- data.frame(sample = 1:10,
                    stimulus = letters[1:10],
                    response = 1:10)
  expect_error(check_trace(df2), "`stimulus` must be a numeric vector.")

  df3 <- data.frame(sample = 1:10,
                    stimulus = 1:10,
                    response = letters[1:10])
  expect_error(check_trace(df3), "`response` must be a numeric vector.")
})

test_that("checks for samples and peaks work", {
  # Good ones work
  expect_silent(check_samples(x))
  expect_silent(check_samples(y))

  expect_error(check_samples("hello"),
               "<peaks> must by a numeric vector or list of numeric vectors.")
  expect_error(check_samples(list("hello", 1:5), peaks = FALSE),
               "<samples> must by a numeric vector or list of numeric vectors.")
})

test_that("checks for peak and stimuli length work", {
  # Good ones work
  expect_silent(check_lengths(y, x))
  expect_silent(check_lengths(1, y))

  # peaks is a list
  expect_error(check_lengths(y[1:5], x))
  # peaks is a vector
  expect_error(check_lengths(y, y))
})
