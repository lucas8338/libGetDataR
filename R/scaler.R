# this library has methods to scaling data

library(dplyr)
library(glue)

setClass("scaler.min_max", representation(stat = "data.frame"))
setMethod("predict", "scaler.min_max",
          function(object, data){
            for (column in colnames(object@stat)){if (!column %in% colnames(data)){stop(glue::glue("column: {column} was there in the fitted data, but dont there in this data, is missing a column in the data"))}}
            for (column in colnames(object@stat)){
              min <- object@stat['min', column]
              max <- object@stat['max', column]
              data[[column]] <- lapply(data[, column], FUN = function(x){(x - min) / (max - min)}) %>% unlist()
            }
            data
          }
)

#' @title scaler.min_max: a min_max scaler
#' @param data: is the data to be applied the transformation
#' @param column: a vector/list wtth the names of columns to apply the transformation
#' @return: return a instance of class 'scaler.min_max' to use the 'predict' method
#' @export
scaler.min_max <- function(data, columns){
  stat <- data.frame(row.names = c('min', 'max'))
  for (column in columns){
    stat['min', column] <- min(data[, column])
    stat['max', column] <- max(data[, column])
    if (stat['min', column]%>%is.numeric()==FALSE | stat['max', column]%>%is.numeric()==FALSE){
      stop(glue("some value of the column: {column} is not numeric"))
    }
  }
  new("scaler.min_max", stat = stat)
}
