#' this file contains functions to preprocess data.frames compatible
library(dplyr)
library(tidyr)

#' this function will transform the dataframe with missing data in exactly espaced data
#' this receive a data.frame in this 'data' argument and a vector in the 'index' argument
df_asfreq <- function(data, index){
  data['asfreq_column'] <- rownames(data) %>% as.numeric()
  data <- tidyr::complete(data, asfreq_column = index)
  data <- as.data.frame(data)
  rownames(data) <- data[, 'asfreq_column']
  data <- data[, -which(colnames(data) == 'asfreq_column')]
  stopifnot(nrow(data) == length(index))
  data
}