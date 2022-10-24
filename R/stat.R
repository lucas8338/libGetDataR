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

#' @title stat.billcorr: binary logical leading correlation.
#' @description this will do a 'binary' correlation to whether or not exog can predit
#' the endog, to explain the idea under this functions is simple, imagine an air-cooler
#' you increase the power of this air-cooler (reduce the target temperature), for example, to
#' 17 degrees, you know the final temperature of the room will be 17 degrees, but you
#' dont know which temperature will be after 10 seconds, cause the temperature for the next
#' 10 seconds will consider a lot of factors, like, external temperature, and anothers. but you
#' know the temperature will fall. This iw what this function is able to predict
#' this get the information whether the value of endog is raised from the last row and
#' will check if the bar has raised or fall in endog, if is raised too this will set a falue of
#' 1 or -1 if falled instead, 0 for unconsider (like NA's), at end the bigger of -1 or 1 will
#' taken and a percentage calculation will be done returning the result, so as you saw -1 means
#' the correlation between exog predict endog is oposite, if the exog rises, the endog falls,
#' so the percentage will be negative.
#' @param data a dataframe containing the data
#' @param step is a multipler how much bars take to do a guess, for example a 'step=32' this mean:
#' 'check if the lasts 32 rows of endog can predict what will happen along of the nexts 32 rows
#' of endog'. so if the param 'lag=1' the lag will become 32 cause the data will be taken at
#' chunks of 32.
#' @param endog.columns the columns i want to check if some of the exog columns can predict
#' @param exog.columns columns i want check if these can predicts endog.
#' @param lags a vector or single value to lags
#' @param threads.num the number of threads to do in parallel through foreach
#' @param threads.type the type of thread, can be for now "PSOCK (windows) or FORK (unix-like)."
#' @return a dataframe containing the percent of 'how much the exog has predicted the binary direction (rise or fall)
#' of the endog column, there too statistics about the columns, accessing the atributes of the column: "attributes(df[['column']])".
#' this will return a data.frame containing info of 'sameDirection' (endog was in the same direction than exog);
#' 'reverseDirection' (endog was in oposite direction of exog); 'total' (the sum of 'sameDirection' and 'reverseDirection')
#' this (total) is indented to used as a information o how much valid values.
#' @useDynLib libGetDataR
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
stat.bllcorr<- function(data,step=1,endog.columns=colnames(data),exog.columns=colnames(data),lags=1:50,threads.num=parallel::detectCores(),threads.type="PSOCK"){
  # register the cluster for paralellization through foreach
  cl<- parallel::makeCluster(threads.num,type = threads.type)
  doSNOW::registerDoSNOW(cl)
  on.exit(parallel::stopCluster(cl))

  # check if the nrow is divisible by step
  if ( step!=1 && nrow(data)%%step!=0 ){
    data<- data[((1+(nrow(data)%%step)):(nrow(data))),]
  }
  stopifnot(nrow(data)%%step==0)

  # generates a sequence which rows i want for the data
  seqFilterDataframe<- seq(from=1,to=nrow(data),by=step)
  # get only the rows i want (setted upper) and the columns will be used
  # (endog.columns+exog.columns)
  data<- data[seqFilterDataframe,unique(c(endog.columns,exog.columns))]
  # declare a variable containing all combinations of columns endog and exog
  # separated by '->'
  print('step 1 of 3')
  columnsCombinations<- foreach::foreach(dotColumn=endog.columns,.combine='c')%dopar%{

    # will store the combination for each endog column
    combs<-c()
    for (..column in exog.columns){
      combs<- append(combs,glue::glue("{..column}->{dotColumn}"))
    }
    # return the combs for each iteration of foreach's loop
    combs
  }
  # generate the results if the price/value is up or down for each column
  pg.it<- length(colnames(data))
  pg<- util.generateForeachProgressBar(pg.it)
  columnsHistory<- foreach::foreach(dotColumn=colnames(data),.packages = c('Rcpp'),.options.snow=pg)%dopar%{
    result<- stat_bllcorr_downOrUp(data[[dotColumn]])
    list(column=dotColumn,result=result)
  }
  # creates a data.frame to storage better way the results from the list 'columnsHistory'
  columnsHistoryDf<- data.frame( row.names = rownames(data) )
  for ( item in columnsHistory ){
    columnsHistoryDf[[item[['column']]]]<- item[['result']]
  }
  # bellow will set the variable 'columnsHistory' by the columnsHistoryDf
  # cause it is a data.frame and will trigger the garbage colector to free ram
  columnsHistory<- columnsHistoryDf
  invisible(gc())
  # bellow wil process the if or not the exog was capable to predict the endog
  print('step 2 of 3')
  pg.it<- length(columnsCombinations)
  pg<- util.generateForeachProgressBar(pg.it)
  lagsResults<- foreach::foreach(dotComb=columnsCombinations, .packages = c('Rcpp'),.options.snow=pg)%dopar%{
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
      res<- stat_bllcorr_doesExogPredictsEndogCateg(exog=..exog,endog=..endog)
      laggedRes[['column']]<- dotComb
      laggedRes[['result']][[as.character(.lag * step)]]<- res
    }
    laggedRes
  }
  # there something interesting at this point, at this point with the variable
  # 'lagsResults' i can expand the funcionality and do what i have planned
  # get the statics to identify wheter has occured a 'seasonal' like effect

  # bellow will collect the results from 'lagsResults' and calculate the percent
  # tusing a 'table' then will assign the percent of each lag to their row/column
  # in a data.frame and will return a data.frame
  print('step 3 of 3')
  pg.it<- length(lagsResults)
  pg<- util.generateForeachProgressBar(pg.it)
  endDf<- foreach::foreach( dotItem=lagsResults,.combine = cbind,.options.snow=pg )%dopar%{
    endDf<- data.frame()
    # this is a metadata will be added to each column containing the stat
    # of the result
    endDf.metadata<- data.frame()
    column<- dotItem[['column']]
    for ( itemName in names(dotItem[['result']]) ){
      statistic<- table(dotItem[['result']][[itemName]])
      # the bellows variables get return from a tryCatch cause in table some items
      # can not have all values '1' or '-1'
      sameDirection<- tryCatch(statistic[['1']],error = function (e){0})
      reverseDirection<- tryCatch(statistic[['-1']],error = function(e){0})
      total<- sameDirection + reverseDirection
      bigger<- max(c(sameDirection,reverseDirection))
      percent<- bigger / total
      endDf[itemName,column]<- ifelse(sameDirection>=reverseDirection,percent,-percent)
      endDf.metadata[itemName,'sameDirection']<- sameDirection
      endDf.metadata[itemName,'reverseDirection']<- reverseDirection
      endDf.metadata[itemName,'total']<- total
    }
    # add the metadata to the column
    attr(endDf[[column]],'stat')<- endDf.metadata
    endDf
  }
  endDf
}