## Test waveMC to tibble ##

wav <- ex_waveMC
tbl <- ex_trace_tbl

test_that("wave to tibble works", {
  expect_equal(waveMC_to_tbl(wav, freq = 10000), tbl)
  expect_equal(names(waveMC_to_tbl(wav, freq = 10000)),
               c("sample", "secs", "stimulus", "response"))
  expect_gt(max(waveMC_to_tbl(wav, freq = 10000)$stimulus),
            max(waveMC_to_tbl(wav, freq = 10000)$response))

  # Error
  expect_error(waveMC_to_tbl(tbl, freq = 10000),
               "<wav> must be an object of class <WaveMC>.")
})
