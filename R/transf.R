# contains functions to trensformate data.

#' @title transf.diff
#' @inherit base::diff
#' @param data a numeric vector.
#' @export
transf.diff <- function(data, lag=1, differences=1){
  ndata<- diff(data,lag=lag,differences = differences)
  while (length(ndata)<length(data)){
    ndata<- append(ndata,NA,after = 0)
  }
  ndata
}

#' @title transt.pctChange
#' @description get the change in percent from atual to the previous data
#' @param data a numeric vector.
#' @param lag a integer containing the lag.
#' @return a vector with the result in percent from 0 to 1.
#' @export
transf.pctChange<-function(data,lag=1){
  times<- data.frame(original=data,lagged=dplyr::lag(data,n=lag))
  result<- (times[['original']] - times[['lagged']])/times[['lagged']]
  result
}
