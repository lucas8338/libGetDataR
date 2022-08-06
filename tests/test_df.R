library(testthat)
source('./R/df.R')

data.asfreq <- readRDS('./tests/test_df_asfreq.rds')
testthat::expect_equal(df_asfreq(data.asfreq[[1]][[1]], data.asfreq[[1]][[2]]), data.asfreq[[2]])