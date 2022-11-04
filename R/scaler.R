# this library has methods to scaling data

library(dplyr)
library(glue)

#' @title scaler.min_max: a min_max scaler
#' @param data is the data to be applied the transformation
#' @param columns a vector/list wtth the names of columns to apply the transformation
#' @return: return a instance of class 'scaler.min_max' to use the 'predict' method
#' @export
scaler.min_max <- function(data, columns=NULL){
  if ( is.null(columns) ){columns<- colnames(data)}

  for ( column in columns ){
    if ( is.numeric(data[[column]])==FALSE ){
      stop(glue::glue("the column: {column} is not numeric. All columns need to be numeric and have'nt NA"))
    }
  }

  stopifnot("there NA in the data"=any(is.na(data[,columns]))==FALSE)

  stat <- data.frame(matrix(nrow = 2,ncol = length(columns)))
  rownames(stat)<- c('min','max')
  colnames(stat)<- columns
  for (column in columns){
    stat[c('min','max'), column] <- c( min(data[[column]]) , max(data[[column]]) )
  }

  result<- list(stat=stat,columns=columns)

  class(result)<- 'scaler.min_max'

  result
}

#' @exportS3Method
predict.scaler.min_max<- function(fitted,data){
  stat<- fitted[['stat']]
  columns<- fitted[['columns']]

  stopifnot("there columns are different from the fitted"=any(colnames(data) %in% columns==FALSE)==FALSE)

  for ( column in columns ){
    min<- stat['min',column]
    max<- stat['max',column]
    data[[column]]<- ( data[[column]] - min )/( max - min )
  }

  data
}