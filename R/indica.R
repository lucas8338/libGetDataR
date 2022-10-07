# thisfile contains indicators, them are pseudo-mathematical formulas to transform data

#' @title indica.supertrend: supertrend indicator a indicator for trend
#' @description this is a trend indicator, this is very smooth taken from site: "https://tradingfuel.com/supertrend-indicator-formula-and-calculation/".
#' @param high the high price of candlestick
#' @param low the low price of candlestick
#' @param close the close price of candlestick
#' @param multi a double, is a multiplicator of the formula
#' @param atr.n<- the 'n' value param of the atr, used in formula
#' @return a data.frame containing 2 columns 'upperBand' and 'lowerBand'
#' @export
indica.supertrend<- function(high,low,close,multi=1,atr.n=14){
  hlc<- matrix(c(high,low,close),ncol = 3)
  colnames(hlc)<- c('high','low','close')
  atr<- TTR::ATR(hlc,n=atr.n)[,'atr']
  upperBand<- (high+low)/2+multi*atr
  lowerBand<- (high+low)/2-multi*atr
  resultDf<- data.frame(upperBand=upperBand,lowerBand=lowerBand)
  resultDf
}