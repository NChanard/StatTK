#' SqrtNorm
#'
#' Normalize data by square root and adding a constant value
#' @param x.num <numeric>: Numerical vector
#' @param a.num <numeric>: Constant value to add before compute square root (Default 0)
#' @return Normalized vector
#' @examples
#' set.seed(542972)
#' x.num <- rnbinom(1000, 1, 0.01)
#' pdf(file=paste0(getwd(),"/Rplots.pdf"))
#'     plot(density(x.num))
#'     plot(density(SqrtNorm(x.num, abs(min(x.num)))))
#' dev.off()
SqrtNorm <- function(x.num, a.num=0){(x.num+a.num)**(1/2)}