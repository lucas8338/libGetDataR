# this file contains function to work with statistics

#' @title stat.ccm: coss-correlation matrix
#' @description this function takes a data.frame and return as data.frame
#' with lags and all possibilities to predict, in columns you will see 'column1'->'column2'
#' this mean that 'column1' predics 'column2', and for example, the best value is in 50,
#' this mean that 'column1' laged to 50 ahead ( to the future ) predics the next value of the 'column2'
#'
#' @param data the data.frame
#' @param endog.columns column to be used as y, among all columns, another words the columns in this parameters
#' will be like 'endogenous' columns
#' @param exog.columns columns to be use as x
#' @param lag.max the maximum value of lag to consider
#' @param lag.min the minium value of lag, the data will use all possible combinations
#' so negative lag is not needed
#' @param na.action what to do about values with NA, gets a function uses the stats packages, example: stats::na.exclude
#' @param threads the number of processes to run in parallel through foreach package
#' @param threads_type the type of threads for the foreach package can be 'PSOCK' or 'FORK', default: 'PSOCK'
#' 'FORK' cant be used on windows
#' @import doParallel
#' @import foreach
#' @import parallel
#' @import dplyr
#' @export
stat.ccm<- function(data,endog.columns=colnames(data),exog.columns=colnames(data),lag.max=50,lag.min=0,na.action=stats::na.pass,threads=parallel::detectCores(),threads_type='PSOCK'){

  cl<- parallel::makeCluster(threads,type=threads_type)
  doParallel::registerDoParallel(cl=cl)

  lags<- c(lag.min:lag.max)
  result<- data.frame(row.names = lags)

  col.comb<- foreach::foreach( excolumn=exog.columns,.combine = 'c' )%dopar%{
    col.intcomb<-c()
    for ( endcolumn in endog.columns ){
      col.intcomb<- append(col.intcomb,glue::glue("{excolumn}->{endcolumn}"))
    }
    col.intcomb
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

#' @title stat.billcor: still doing
#' @useDynLib libGetDataR
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
stat.bllcorr<- function(data,step=1,endog.columns=colnames(data),exog.columns=colnames(data),lags=1:50,threads.num=parallel::detectCores(),threads.type="PSOCK"){
  # register the cluster for paralellization through foreach
  cl<- parallel::makeCluster(threads.num,type = threads.type)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  # generates a sequence which rows i want for the data
  seqFilterDataframe<- seq(from=1,to=nrow(data),by=step)
  # get only the rows i want (setted upper) and the columns will be used
  # (endog.columns+exog.columns)
  data<- data[seqFilterDataframe,unique(c(endog.columns,exog.columns))]
  # declare a variable containing all combinations of columns endog and exog
  # separated by '->'
  columnsCombinations<- foreach::foreach(dotColumn=endog.columns,.combine='c')%dopar%{
    # will store the combination for each endog column
    combs<-c()
    for (..column in exog.columns){
      combs<- append(combs,glue::glue("{..column}->{dotColumn}"))
    }
    # return the combs for each iteration of foreach's loop
    combs
  }
  columnsCombinations<<- columnsCombinations
  # generate the results if the price/value is up or down for each column
  columnsHistory<- foreach::foreach(dotColumn=colnames(data),.packages = c('Rcpp'))%dopar%{
    result<- stat_bllcorr_downOrUp(data[[dotColumn]])
    list(column=dotColumn,result=result)
  }
  # creates a data.frame to storage better way the results from the list 'columnsHistory'
  columnsHistoryDf<- data.frame( row.names = 1:(nrow(data)) )
  for ( item in columnsHistory ){
    columnsHistoryDf[[item[['column']]]]<- item[['result']]
  }
  # bellow will set the variable 'columnsHistory' by the columnsHistoryDf
  # cause it is a data.frame and will trigger the garbage colector to free ram
  columnsHistory<- columnsHistoryDf
  columnsHistory<<- columnsHistory
  invisible(gc())
  # bellow wil process the if or not the exog was capable to predict the endog
  lagsResults<- foreach::foreach(dotComb=columnsCombinations, .packages = c('Rcpp'))%do%{
    splittedName<- unlist(stringr::str_split(dotComb,pattern = '->'))
    exog.name<- splittedName[[1]]
    endog.name<- splittedName[[2]]
    .exog<- columnsHistory[[exog.name]]
    .endog<- columnsHistory[[endog.name]]
    # store the result of each lag of each column
    laggedRes<- list()
    for ( .lag in lags ){
      ..exog<- dplyr::lag(.exog,n=as.integer(.lag))
      ..endog<- .endog
      print(exog.name)
      print(endog.name)
      res<- stat_bllcorr_doesExogPredictsEndogCateg(exog=..exog,endog=..endog)
      laggedRes[[dotComb]][[.lag]]<- res
    }
    laggedRes
  }

  lagsResults<<- lagsResults
  stop()

}