#' this file contains functions to preprocess data.frames compatible
library(dplyr)
library(tidyr)

#' this function will transform the dataframe with missing data in exactly espaced data
#' this receive a data.frame in this 'data' argument and a vector in the 'index' argument
df.asfreq <- function(data,freq){
  rn<- rownames(data)%>%as.numeric()
  index<- seq(from = rn[1],to=rn[length(rn)],by=freq)
  ndata<-data
  ndata['asfreq_column'] <- rownames(data) %>% as.numeric()
  ndata <- tidyr::complete(ndata, asfreq_column = index)
  ndata <- as.data.frame(ndata)
  rownames(ndata) <- ndata[, 'asfreq_column']
  ndata <- ndata[, -which(colnames(ndata) == 'asfreq_column')]
  stopifnot(nrow(ndata) == length(index))
  stopifnot(sum(is.na(ndata))>=(length(index)-nrow(data))*ncol(data))
  stopifnot(sum(!is.na(ndata))==sum(!is.na(data)))
  ndata
}