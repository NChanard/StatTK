% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TrimOutliers.r
\name{TrimOutliers}
\alias{TrimOutliers}
\title{Remove outliers.}
\usage{
TrimOutliers(x.num, tresholds.num = SdThreshold(x.num), clip.bln = FALSE)
}
\arguments{
\item{x.num}{<numeric>: numeric vector.}

\item{tresholds.num}{<numeric>: numeric vector of length 2. first value is minimal threshold, second value maximal threshold (Default find threshold based on standarrd deviation. see `SdThreshold` function)}

\item{clip.bln}{<logical>: if TRUE the value out of bounds are replace with threshodls values. If FALSE the Values out of bound are replace with NA (Default FALSE).}
}
\value{
trimed Numerical vector.
}
\description{
Replace values of a numerical vector that are below a minimal thresholds and/or above maximal thresholds.
}
\details{
TrimOutliers
}
\examples{
set.seed(1111)
x.num = rnorm(1000)
x.num = sort(x.num)
x.num[990:1000]
SdThreshold(x.num)
TrimOutliers(x.num)[990:1000]
TrimOutliers(x.num, clip=TRUE)[990:1000]
}
