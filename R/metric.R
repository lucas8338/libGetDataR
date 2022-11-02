# this file contains functions to extract machine learning metrics, cause
# the two packages in R that does this, dont work very well in my optinion
# two them 'MLmetrics' and 'Metrics' you got different results if your
# data is integer or factors, this library will fix this

#' @title metric.precision
#' @description calculates the precision "TP/(TP+FP)"
#' @param true a logical vector containing the correct answer
#' @param predicted a logical vector containing the predict
#' @return a double with precision
#' @export
metric.precision<- function(true,predicted){
  stopifnot("'true' is not logical"=is.logical(true))
  stopifnot("'predicted' is not logical"=is.logical(predicted))

  stopifnot("length of 'true' is different of 'predicted'"=length(true)==length(predicted))

  if ( any(is.na(true)) || any(is.na(predicted)) ){
    warning("there NA's in the inputs, NA's are not considered")
  }

  if ( all(true==TRUE) || all(true==FALSE) ) warning("'all values in 'true' are the same!")
  if ( all(predicted==TRUE) || all(predicted==FALSE) ) warning("'all values in 'predicted' are the same!")

  TP<- 0

  FP<- 0

  for ( i in 1:(length(predicted)) ){
    if ( predicted[[i]]==TRUE && true[[i]]==TRUE ) TP<- TP+1
    if ( predicted[[i]]==TRUE && true[[i]]==FALSE ) FP<- FP+1
  }

  calc<- TP/(TP+FP)

  calc

}

#' @title metric.recall
#' @description calculates recall "TP/(TP+FN)"
#' @param true a logical vector containing the answer
#' @param predicted a logical vector contaning the predictions
#' @return a double with result
#' @export
metric.recall<- function(true,predicted){
  stopifnot("'true' is not logical"=is.logical(true))
  stopifnot("'predicted' is not logical"=is.logical(predicted))

  stopifnot("length of 'true' is different of 'predicted'"=length(true)==length(predicted))

  if ( any(is.na(true)) || any(is.na(predicted)) ){
    warning("there NA's in the inputs, NA's are not considered")
  }

  if ( all(true==TRUE) || all(true==FALSE) ) warning("'all values in 'true' are the same!")
  if ( all(predicted==TRUE) || all(predicted==FALSE) ) warning("'all values in 'predicted' are the same!")

  TP<- 0

  FN<- 0

  for ( i in 1:(length(predicted)) ){
    if ( predicted[[i]]==TRUE && true[[i]]==TRUE ) TP<- TP+1
    if ( predicted[[i]]==FALSE && true[[i]]==TRUE ) FN<- FN+1
  }

  calc<- TP/(TP+FN)

  calc

}

#' @title metric.f1
#' @description calculates the f1 score "(2*precision*recall)/(precision+recall)"
#' @param true a logical vector containing the answers
#' @param predicted a logical vector containing the predictions
#' @return a double
#' @export
metric.f1<- function(true,predicted){
  stopifnot("'true' is not logical"=is.logical(true))
  stopifnot("'predicted' is not logical"=is.logical(predicted))

  stopifnot("length of 'true' is different of 'predicted'"=length(true)==length(predicted))

  if ( any(is.na(true)) || any(is.na(predicted)) ){
    warning("there NA's in the inputs, NA's are not considered")
  }

  precision<- metric.precision(true,predicted)

  recall<- metric.recall(true,predicted)

  calc<- (2*precision*recall)/(precision+recall)

  calc

}


#' @title metric.accuracy
#' @description calculates accuracy this way: length(which(predicted==true))/length(true)
#' @param true a vector containing the corrects
#' @param predicted a vector containing the predictions
#' @return a double containing the result
metric.accuracy<- function(true,predicted){
  stopifnot("'true' and 'predicted' are'nt of the same typeof"=typeof(true)==typeof(predicted))
  stopifnot("the size of 'true' and 'predicted' are different"=length(true)==length(predicted))

  calc<- length(which(predicted==true))/length(true)

  calc
}