#' Scalers to reescaling the data
#'
#' @details
#'    - scaler.min_max: a mix-max scalling using the formula x=(x-min)/(max-min); x is the actual value of a column
#'      of a data.frame
#'



library(dplyr)
library(glue)

setClass("scaler.min_max", representation(stat = "data.frame"))
setMethod("predict", "scaler.min_max",
          function(object, data) {
            for (column in colnames(object@stat)) { if (!column %in% colnames(data)){stop(glue::glue("column: {column} was there in the fitted data, but dont there in this data, is missing a column in the data"))}}
            for (column in colnames(object@stat)) {
              min <- object@stat['min', column]
              max <- object@stat['max', column]
              data[column] <- lapply(data[, column], FUN = function(x) { (x - min) / (max - min) }) %>% unlist()
            }
            data
          }
)

#' @param data: is the data to be applied the transformation
#' @param column: a vector/list wtth the names of columns to apply the transformation
#' @return: return a instance of class 'scaler.min_max' to use the 'predict' method
#'
scaler.min_max <- function(data, columns) {
  stat <- data.frame(row.names = c('min', 'max'))
  for (column in columns) {
    stat['min', column] <- min(data[, column])
    stat['max', column] <- max(data[, column])
  }
  new("scaler.min_max", stat = stat)
}
