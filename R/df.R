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

#' @title df.mlSafeDropNa: drop safely NA values only from start
#' @description this function removes NA's values but differently from anothers NA's removers
#' this will not to remove NA's that are not at start of the data.frame, so the data in the middle
#' of data.frame is safe
#' @param df a data.frame
#' @return a data.frame
#' @export
df.mlSafeDropNa<-function(df){
  for ( rn in 1:(nrow(df)) ){
    if ( any(is.na(df[rn,]))==FALSE ){
      df<- df[rn:(nrow(df)),]
      break
    }
  }
  df
}