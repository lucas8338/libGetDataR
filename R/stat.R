# this file contains function to work with statistics

#' @title stat.ccm: coss-correlation maxtrix
#' @description this function takes a data.frame and return as data.frame
#' with lags and all possibilities to predict, in columns you will see 'column1'->'column2'
#' this mean that 'column1' predics 'column2', and for example, the best value is in 50,
#' this mean that 'column1' laged to 50 ahead ( to the future ) predics the next value of the 'column2'
#'
#' @param data the data.frame
#' @param lag.max the maximum value of lag to consider
#' @param lag.min the minium value of lag, the data will use all possible combinations
#' so negative lag is not needed
#' @param na.action what to do about values with NA, gets a function uses the stats packages, example: stats::na.exclude
#' @param threads the number of processes to run in parallel through foreach package
#' @param threads_type the type of threads for the foreach package can be 'PSOCK' or 'FORK', default: 'PSOCK'
#' 'FORK' cant be used on windows
#' @export
stat.ccm<- function(data,lag.max=50,lag.min=0,na.action=stats::na.pass,threads=parallel::detectCores(),threads_type='PSOCK'){
  cl<- parallel::makeCluster(threads,type=threads_type)
  doParallel::registerDoParallel(cl=cl)

  col.comb<- c()
  lags<- c(lag.min:lag.max)
  result<- data.frame(row.names = lags)

  for ( n.column in 1:(length(colnames(data))) ){
    for ( n.intcolumn in 1:(length(colnames(data))) ){
      if ( n.intcolumn==n.column ){
        next
      }else{
        col.comb<- append(col.comb,glue::glue("{colnames(data)[[n.column]]}->{colnames(data)[[n.intcolumn]]}"))
      }
    }
  }

  .process<- foreach::foreach( comb=col.comb,.packages = 'dplyr' )%dopar%{
    s<- stringr::str_split(comb,'->')%>%unlist()
    x<- s[[1]]
    y<- s[[2]]
    .ccf<- ccf(x=data[[x]],y=data[[y]],lag.max = lag.max,type='correlation',plot=FALSE,na.action = na.action)
    .ccf<- data.frame(row.names = .ccf[['lag']],'cross_corr'=.ccf[['acf']])
    .ccf<- .ccf[lags%>%as.character(),]
    list(comb,.ccf)
  }
  parallel::stopCluster(cl)

  for ( i in .process){
    result[[i[[1]]]]<- i[[2]]
  }
  result
}