% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RobustScalarScale.r
\name{RobustScalarScale}
\alias{RobustScalarScale}
\title{Robust scalar sclaing.}
\usage{
RobustScalarScale(x.num)
}
\arguments{
\item{x.num}{<numeric>: numerical vector.}
}
\value{
scaled numeric vector.
}
\description{
Scale values with robust scalar method.
}
\details{
RobustScalarScale
}
\examples{
set.seed(655213)
x.num <- rnorm(500,500)
set.seed(522613)
y.num <- rnorm(500,100)
plot(density(x.num),col="red",xlim=c(min(y.num),max(x.num )))
lines(density(y.num),col="green")
plot(density(RobustScalarScale(x.num)),
     col="red",
     xlim=c(
         min(RobustScalarScale(y.num)),
         max(RobustScalarScale(x.num) )))
lines(density(RobustScalarScale(y.num)),col="green")
}
