#' MeanScale
#'
#' Scale values with mean.
#' @param x.num <numeric>: Numerical vector
#' @return Scaled numeric vector
#' @examples
#' set.seed(655213)
#' x.num <- rnorm(500,500)
#' set.seed(522613)
#' y.num <- rnorm(500,100)
#' pdf(file=paste0(getwd(),"/Rplots.pdf"))
#'     plot(density(x.num),col="red",xlim=c(min(y.num),max(x.num )))
#'     lines(density(y.num),col="green")
#'     plot(density(MeanScale(x.num)),col="red",xlim=c(min(MeanScale(y.num)),max(MeanScale(x.num) )))
#'     lines(density(MeanScale(y.num)),col="green")
#' dev.off()
MeanScale <- function(x.num){(x.num-mean(x.num,na.rm=TRUE))/(max(x.num,na.rm=TRUE)-min(x.num,na.rm=TRUE))}