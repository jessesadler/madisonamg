## Test find values from samples ##

tbl <- ex_trace_tbl

test_that("find amplitude values of response works", {
  expect_equal(find_values(tbl, 35331), 128)
  expect_equal(find_values(tbl, list(35331L, 45385L)), list(128, 192))
  expect_equal(find_values(tbl, list(35331, 45385)), list(128, 192))

  # Errors
  expect_error(find_values(tbl, list(1000:3000, "a")),
               "<samples> must be a numeric vector or list of numeric vectors.")
  expect_error(find_values(tbl, "a"))
})
