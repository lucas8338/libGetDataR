#' split a timeseries data (data.frame) in to a x/y to fit a model
library(parallel)
library(doParallel)
library(foreach)
library(svMisc)

#' A function to split a dataframe into x and y
#' remember the y is still a dataframe need to be processed to fit
#' @param data
#' @param input_size
#' @param output_size
#' @param n_jobs
#' @return list(data.x,data.y)
split.x_y_timeseries_split <- function(data, input_size, output_size, auto_resize = TRUE){
  sequence_size <- input_size + output_size
  if (nrow(data) %% sequence_size != 0){
    if (auto_resize == TRUE){
      data <- data[((nrow(data) %% sequence_size) + 1):nrow(data),]
    }else{
      stop(glue::glue("the nrow of the data i not divisible by {sequence_size},
       you can set the parameter 'auto_resize=TRUE' or solve this manualy"))
    }
  }
  data.x <- list()
  data.y <- list()
  iters <- 1:((nrow(data) - sequence_size) + 1)

  process<-function (i){
    svMisc::progress(i, length(iters))
    visible.data <- data[i:(sequence_size + (i - 1)),]
    x<- visible.data[1:input_size,]
    y<- visible.data[(input_size + 1):nrow(visible.data),]
    list(x=x,y=y)
  }
  result<-list()
  for (i in iters){
    p<- process(i)
    result[['x']]<-append(result[['x']],list(p[['x']]))
    result[['y']]<-append(result[['y']],list(p[['y']]))
  }
  result
}
