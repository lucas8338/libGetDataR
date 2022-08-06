library(testthat)
library(dplyr)
source('./R/scaler.R')

data.min_max<-readRDS(file = './tests/test_scaler.min_max.rds')
model<- scaler.min_max(data.min_max[['x']],columns = colnames(data.min_max[['x']])[which(colnames(data.min_max[['x']])!='missing')])
testthat::expect_equal(predict(model,data.min_max[['x']]),data.min_max[['y']])