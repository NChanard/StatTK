#' Robust scalar sclaing.
#'
#' RobustScalarScale
#' @description Scale values with robust scalar method.
#' @param x.num <numeric>: numerical vector.
#' @return scaled numeric vector.
#' @examples
#' set.seed(655213)
#' x.num <- rnorm(500,500)
#' set.seed(522613)
#' y.num <- rnorm(500,100)
#' plot(density(x.num),col="red",xlim=c(min(y.num),max(x.num )))
#' lines(density(y.num),col="green")
#' plot(density(RobustScalarScale(x.num)),
#'      col="red",
#'      xlim=c(
#'          min(RobustScalarScale(y.num)),
#'          max(RobustScalarScale(x.num) )))
#' lines(density(RobustScalarScale(y.num)),col="green")
RobustScalarScale <- function(x.num){(x.num-stats::median(x.num,na.rm=TRUE))/stats::quantile(x.num,0.75,na.rm=TRUE)-stats::quantile(x.num,0.25,na.rm=TRUE)}