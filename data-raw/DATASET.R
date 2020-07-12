## code to prepare `wavMC_ex` dataset goes here

# Example data for the package

## Create waveMC object ##
ex_waveMC <- tuneR::readWave("data-raw/20200611115711-20161132.wav", toWaveMC = TRUE)

usethis::use_data(ex_waveMC, overwrite = TRUE)

## Create trace tibble ##
ex_trace_tbl <- waveMC_to_tbl(ex_waveMC, freq = 10000)

usethis::use_data(ex_trace_tbl, overwrite = TRUE)
