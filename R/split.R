# this file contains functions to split the data to train test for machine leanring

library(parallel)
library(doParallel)
library(foreach)
library(svMisc)
library(compiler)

#' @title split.x_y_timeseries_split: a function to split a data.frame into train/test
#' @description this function will to split a data.frame into train/test to use it to
#' train a machine learning model, this will just split will not to transform or anything
#' @param data the data
#' @param input_size number the size of input
#' @param output_size number the size of output
#' @param auto _resize whether to remove rows whether data size is not multiple of inputsize*output_size
#' @param wt whether >1 the data will split the generated samples to storage, this only should to be used wheter ram ins insuficient
#' @param wt_dir folder to save the splited samples
#' @param wt_name the name of the parts, this will be evaluated through glue so this can have variables inside '{}'
#' @param wt_compress if or not to compress the generated sample part (compressing will reduce the file size but
#' will take more time to save)
#' @return list(data.x,data.y)
#' @export split.x_y_timeseries_split
split.x_y_timeseries_split <- function(data, input_size, output_size, auto_resize = TRUE, wt = 0, wt_dir = '', wt_name= "part{i}.rds", wt_compress = FALSE){
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

  pg<- progress::progress_bar$new(total=length(iters),format=util.progress_format())

  process<-function (i){
    pg$tick()
    visible.data <- data[i:(sequence_size + (i - 1)),]
    x<- visible.data[1:input_size,]
    y<- visible.data[(input_size + 1):nrow(visible.data),]
    list(x=x,y=y)
  }
  cproc<-compiler::cmpfun(process)
  result<-list()
  if ( wt > 0 ){
    part<- c(length(iters))
    for (i in iters){
      if ( i %% wt == 0 ){
        part<- append(part, i, after = 0)
      }
    }
  }
  for (i in iters){
    p<- cproc(i)
    result[['x']]<-append(result[['x']],list(p[['x']]))
    result[['y']]<-append(result[['y']],list(p[['y']]))
    if (wt > 0){
      if ( i %in% part ){
        saveRDS(result,file = glue::glue("{wt_dir}/{eval(glue::glue(wt_name))}"),compress=wt_compress)
        rm(result)
        result<- list()
        invisible(gc())
      }
    }
  }
  result
}
