% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{stat_bllcorr_downOrUp}
\alias{stat_bllcorr_downOrUp}
\title{stat_bllcorr_downOrUp: has the price fall or rised compaired to the previous price?}
\usage{
stat_bllcorr_downOrUp(x)
}
\arguments{
\item{x}{the a NumericVector with the values}
}
\value{
a NumeriVector with the same size that the 'x' parameter with the values:
-1 (has fallen), 0 (unconsider), 1 (has raisen).
}
\description{
bellow is a function that gets a numeric vector and return whether the value
has down or up compairing with their previous value
if the any of the compairing values is NA the value is 0 ( unconsider ), 1 for has up
-1 for has down
}
