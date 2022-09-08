# this file contains functions to expand the data.frame compatible functionalities

library(dplyr)
library(tidyr)

#' @title df.asfreq: equaly spaced data.frame
#' @description this function will turn a data.frame with missing dates in a equaly spaced
#' data.frame, with NA in the new datas
#' @param data the data.frame
#' @param freq number the frequency
#' @return data.frame
#' @export
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