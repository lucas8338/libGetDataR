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
#' @description do concatenation of two data.frame the order of columns will be defined by 'df1', so
#' is recommended for performance to pass the bigger data.frame on df1.
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
df.concat<- function(df1,df2,sort=TRUE){
  df1.colnamesNotInDf2<- colnames(df1)[which(colnames(df1) %in% colnames(df2)==FALSE)]
  df2.colnamesNotInDf1<- colnames(df2)[which(colnames(df2) %in% colnames(df1)==FALSE)]

  # this will add the missing columns that there in a data.frame but not is present in another one.
  df2[df1.colnamesNotInDf2]<- NA
  df1[df2.colnamesNotInDf1]<- NA

  # get a data.frame with the rows of df2 that are not in df1
  df2.notInDf1<- df2[which(rownames(df2) %in% rownames(df1)==FALSE),,drop=FALSE]

  df1<- rbind(df1,df2.notInDf1)

  # add the data of missing columns in df1 that are in df2 to df1
  for ( column in df2.colnamesNotInDf1 ){
    # the two lines of code above 'df2.notInDf1' and the row with rbind
    # make the df1 have all rows in df1 and df2, so will use the rownames
    # of df2 to add and get missing datas
    df1[rownames(df2),column]<- df2[rownames(df2),column]
  }

  if ( sort==TRUE ){
    if ( check.is_characterNumeric(rownames(df1)) ){
      df1<- df1[order(rownames(df1) %>% as.numeric()),,drop=FALSE]
    }else{
      df1<- df1[order(rownames(df1)),,drop=FALSE]
    }
  }

  df1

}