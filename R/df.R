# this file contains functions to expand the data.frame compatible functionalities

library(dplyr)
library(tidyr)

#' @title df.asfreq: equaly spaced data.frame
#' @description this function will turn a data.frame with missing dates in a equaly spaced
#' data.frame, with NA in the new datas
#' @param data the data.frame
#' @param freq number the frequency
#' @return data.frame
#' @export
df.asfreq <- function(data,freq){
  rn<- rownames(data)%>%as.numeric()
  index<- seq(from = rn[1],to=rn[length(rn)],by=freq)
  ndata<-data
  ndata['asfreq_column'] <- rownames(data) %>% as.numeric()
  ndata <- tidyr::complete(ndata, asfreq_column = index)
  ndata <- as.data.frame(ndata)
  rownames(ndata) <- ndata[, 'asfreq_column']
  ndata <- ndata[, -which(colnames(ndata) == 'asfreq_column')]
  stopifnot(nrow(ndata) == length(index))
  stopifnot(sum(is.na(ndata))>=(length(index)-nrow(data))*ncol(data))
  stopifnot(sum(!is.na(ndata))==sum(!is.na(data)))
  ndata
}

#' @title df.mlSafeDropNa: drop safely NA values only from start
#' @description this function removes NA's values but differently from anothers NA's removers
#' this will not to remove NA's that are not at start of the data.frame, so the data in the middle
#' of data.frame is safe
#' @param df a data.frame
#' @param removeFromStart whether is or not to remove NA's from the end
#' @param removeFromEnd whether is or not to remove NA's starting from end of df
#' @return a data.frame
#' @export
df.mlSafeDropNa<-function(df,removeFromStart=TRUE,removeFromEnd=TRUE){
  if ( removeFromStart==TRUE ){
    for ( rn in 1:(nrow(df)) ){
      if ( any(is.na(df[rn,]))==FALSE ){
        df<- df[rn:(nrow(df)),]
        break
      }
    }
  }

  if ( removeFromEnd==TRUE ){
    for ( rn in (nrow(df)):1 ){
      if ( any(is.na(df[rn,]))==FALSE ){
        df<- df[1:rn,]
        break
      }
    }
  }
  df
}

#' @title df.concat
#' @description do concatenation of two data.frame
#' @param df1 first data.frame
#' @param df2 second data.frame
#' @param by can be 'rownames' or a name of column to use to add the missing values of this index.
#' @param if sort (order) the result at end, cause this function uses 'rbind' to concatenate data.frames
#' if a the value can be converted to numeric first, this will be converted to numeric firts.
#' @param addMissingColumns the core of this function will work with 'rbind' this need both data.frames
#' has the same columns. so this will add the missing columns missing columns for each data.frame.
#' the default value for these columns is NA.
#' @return a data.frame
#' @import dplyr
#' @export
df.concat<- function(df1,df2,by='rownames',sort=TRUE,addMissingColumns=TRUE){
  df1.colnames<- colnames(df1)
  df2.colnames<- colnames(df2)

  df1.colnamesNotInDf2<- df1.colnames[which(df1.colnames %in% df2.colnames==FALSE)]
  df2.colnamesNotInDf1<- df2.colnames[which(df2.colnames %in% df1.colnames==FALSE)]

  # this will add the missing columns that there in a data.frame but not is present in another one.
  if ( addMissingColumns==TRUE ){
    for ( name in df1.colnamesNotInDf2 ){
      df2[[name]]<- NA
    }

    for ( name in df2.colnamesNotInDf1 ){
      df1[[name]]<- NA
    }
  }

  # add the value to use to concatenate to a variable
  if ( by=='rownames' ){
    df1.by<- rownames(df1)
    df2.by<- rownames(df2)
  }else{
    df1.by<- df1[[by]]
    df2.by<- df2[[by]]
  }

  df2.notInDf1<- df2[which(df2.by %in% df1.by==FALSE),,drop=FALSE]

  df1<- rbind(df1,df2.notInDf1)

  if ( by=='rownames' ){
    df1.by<- rownames(df1)
  }else{
    df1.by<- df1[[by]]
  }

  # add the data of missing columns in df1 to df1
  # another words the columns which was added in df1 cause them are in df2 but not in df1
  # takes by default NA, so this part of code will iterate over each of them (missing columns)
  # will then select the rownames which were too in both df1 and df2, AND are NA IN DF1, and
  # will set the values of this variable by the values of that column in df2 for these rownames
  for ( name in df2.colnamesNotInDf1 ){
    # get the rownames of values that there in df1 and df2 and are NA in df1
    selection<- df1.by[ which( is.na( df1[ df1.by[ which( df1.by %in% df2.by ) ] %>% as.character(),name ,drop=TRUE] ) ) ]  %>% as.character()
    # set the values in df1
    df1[selection,name]<- df2[selection,name,drop=TRUE]
  }

  if ( sort==TRUE ){
    if ( check.is_characterNumeric(df1.by) ){df1.by<- as.numeric(df1.by)}
    df1<- df1[order(df1.by),,drop=FALSE]
  }

  df1

}