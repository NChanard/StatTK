#' Unit length scaling.
#'
#' UnitLengthScale
#' @description Scale values with unit lenght method.
#' @param x.num <numeric>: numerical vector.
#' @return scaled numeric vector.
#' @examples
#' set.seed(655213)
#' x.num <- rnorm(500,500)
#' set.seed(522613)
#' y.num <- rnorm(500,100)
#' plot(density(x.num),col="red",xlim=c(min(y.num),max(x.num )))
#' lines(density(y.num),col="green")
#' plot(density(UnitLengthScale(x.num)),
#'      col="red",
#'      xlim=c(
#'          min(UnitLengthScale(y.num)),
#'          max(UnitLengthScale(x.num) )))
#' lines(density(UnitLengthScale(y.num)),col="green")
UnitLengthScale <- function(x.num){x.num/sqrt(sum(x.num**2,na.rm=TRUE))}