transf.diff <- function(data, lag=1, differences=1){
  ndata<- diff(data,lag=lag,differences = differences)
  while (length(ndata)<length(data)){
    ndata<- append(ndata,NA,after = 0)
  }
  ndata
}