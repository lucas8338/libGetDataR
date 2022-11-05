# this fiele contains functions to help and speed up coding

#' @title util.generateForeachProgressBar: generates a foreach dopar progressbar
#' @description this functions is based on github answer:
#' https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
#' @param iterations the total number of iterations
#' @return list with a function inside to be passe to the parameter of foreach '.options.snow'
#' @export
util.generateForeachProgressBar<- function(iterations, style=3,...){
  pb <- utils::txtProgressBar(max = iterations, style = style,...)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  opts
}

#' @title util.progress_format
#' @description a format string for general use for progress progress_bar
#' @return a string to pass to 'format' parameter of "progress::progress_bar$new" function
#' @export
util.progress_format<- function(){
  "progress: :current/:total | rate: :tick_rateit/s | elapsed: :elapsedfull | eta: :eta"
}