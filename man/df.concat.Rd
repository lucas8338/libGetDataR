% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.R
\name{df.concat}
\alias{df.concat}
\title{df.concat}
\usage{
df.concat(df1, df2, sort = TRUE)
}
\arguments{
\item{df1}{first data.frame}

\item{df2}{second data.frame}

\item{sort}{the result at end, cause this function uses 'rbind' to concatenate data.frames
if a the value can be converted to numeric first, this will be converted to numeric firts.}
}
\value{
a data.frame
}
\description{
do concatenation of two data.frame by rownames
}
