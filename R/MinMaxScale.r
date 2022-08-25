#' MinMaxScale
#'
#' Scale values on min max bounds.
#' @param x.num <numeric>: Numerical vector
#' @param min.num <numeric>: Minimal value after scaling
#' @param max.num <numeric>: Maximal value after scaling
#' @return Scaled numeric vector
#' @examples
#' set.seed(655213)
#' x.num <- rnorm(500,500)
#' set.seed(522613)
#' y.num <- rnorm(500,100)
#' pdf(file=paste0(getwd(),"/Rplots.pdf"))
#'     plot(density(x.num),col="red",xlim=c(min(y.num),max(x.num )))
#'     lines(density(y.num),col="green")
#'     plot(
#'            density(MinMaxScale(x.num)),
#'            col="red",
#'            xlim=c(min(MinMaxScale(y.num)),max(MinMaxScale(x.num)))
#'     )
#'     lines(density(MinMaxScale(y.num)),col="green")
#' dev.off()
MinMaxScale <- function(x.num, min.num=(0), max.num=1){min.num + (((x.num-min(x.num,na.rm=TRUE))*(max.num-min.num)) / (max(x.num,na.rm=TRUE) - min(x.num,na.rm=TRUE)))}
