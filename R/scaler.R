# this library has methods to scaling data

library(dplyr)
library(glue)

#' @title scaler.min_max: a min_max scaler
#' @description a min-max scaler. a very common scaling thecnique.
#' IS NOT RECOMMENDED TO USE THIS SCALING METHOD TO MACHINE LEARNING PURPOSE!
#' CAUSE IT CAN HAVE LOOKAHEAD BIAS.
#' read: Practical Time Series Analysis, Aileen Nielsen, pag. 421.
#' @section Formula: (x-min) / (max-min).
#' @param data is the data to be applied the transformation
#' @param columns a vector/list wtth the names of columns to apply the transformation
#' @return return a instance of class 'scaler.min_max' to use the 'predict' method
#' @export
scaler.min_max <- function(data, columns=colnames(data)){
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

  stopifnot("there columns in fitted are not in the prediction"=all(columns %in% colnames(data)))

  for ( column in columns ){
    min<- stat['min',column]
    max<- stat['max',column]
    data[[column]]<- ( data[[column]] - min )/( max - min )
  }

  data
}

#' @title aileenScaler
#' @description this scaler came from the book: Practical Time Series Analysis, Aileen Nielsen, pag. 411.
#' this scaler was purpose to solve the problem with lookahead bias induced by the use of
#' statistics of the data, for example, the mean.
#' This scaler uses ema (lookbehind), mean of ema and variance of the ema to scaling the data.
#' IN MY OBSERVATION I CAN SAY IT HAS LOOKAHEAD.
#' the function uses the mean of the ema and var of the ema, but these are estatistics
#' of all data, so the data at begging will have information about the data at
#' end, one of thing can see in the scaled data, is the values bellow the mean
#' of the original data will be negative values, this way i can predict values
#' bellow the mean (negatives one) will rise, cause them are bellow the mean,
#' and values starting with positives will fall.
#' @section formula: (data - mean(ema(data))) / var(ema(data))^0.5
#' @param data the data.frame to do scaling
#' @param halflife.mean the main parameter value for ema will be taken the mean.: mean(ema(data,halflife.mean))
#' @param halflife.var the main parameter value for the ema will be taken the variance.: var(ema(data,halflife.var))
#' @return a list of class 'scaler.aileenScaler' to use the 'predict' method
#' @export
scaler.aileenScaler<- function (df,columns=colnames(df),halflife.mean=10,halflife.var=10,exponential=0.5){
  ema<- function(data,halflife){pracma::movavg(data,halflife,type='e')}

  rmean<- function(data,halflife){pracma::movavg()}

  stat<- data.frame(matrix(nrow = 2,ncol = length(columns)))
  rownames(stat)<- c('ewdf','vewdf')
  colnames(stat)<- columns

  for ( column in columns ){
    data<- df[[column]]

    ewdf<- ema(data,halflife.mean)%>%mean()
    vewdf<- ema(data,halflife.var)%>%var()

    stat[c('ewdf','vewdf'),column]<- c(ewdf,vewdf)

  }

  result<- list(stat=stat,columns=columns,exponential=exponential)

  class(result)<- 'scaler.aileenScaler'

  result

}

#' @exportS3Method
predict.scaler.aileenScaler<- function(model,y){
  columns<- model[['columns']]
  stopifnot("there columns in the fit not in predict time"=all(columns %in% colnames(y)))

  for ( column in columns ){
    data<- y[[column]]
    ewdf<- model[['stat']]['ewdf',column]
    vewdf<- model[['stat']]['vewdf',column]
    exponential<- model[['exponential']]

    scaled<- data - ewdf
    scaled<- scaled / vewdf^exponential

    y[[column]]<- scaled
  }

  y
}

#' @title scaler.expanding_min_max
#' @description do a min_max scaler over a expanding window
#' @param df a data.frame
#' @param columns the names of the columns to apply the scaling
#' @section formula: ( data - min ) / ( max - min )
#' @return a data.frame
#' @export
scaler.expanding_min_max<- function(df,columns=colnames(df)){
  pg<- progress::progress_bar$new(total=length(columns),format=util.progress_format())
  for ( column in columns ){
    pg$tick()
    data<- df[[column]]

    mins<- runner::min_run(data,k=integer(0))
    maxes<- runner::max_run(data,k=integer(0))

    df[[column]]<- ( df[[column]] - mins ) / ( maxes - mins )
  }

  df

}

#' @title scaler.expanding_zScore
#' @description do a standarlization (zscore) over a expanding window
#' @param df a data.frame
#' @param columns the names of the columns to apply the scaling
#' @section formula: ( data - mean ) / standardDeviation
#' @return a data.frame
#' @export
scaler.expanding_zScore<- function(df,columns=colnames(df)){
  pg<- progress::progress_bar$new(total=length(columns),format=util.progress_format())
  for ( column in columns ){
    pg$tick()
    data<- df[[column]]

    means<- runner::mean_run(data,k=integer(0))
    stddevs<- runner::runner(data,f=Rcpp_math_standardDeviation,k=integer(0))

    df[[column]]<- ( df[[column]] - means ) / stddevs
  }

  df

}