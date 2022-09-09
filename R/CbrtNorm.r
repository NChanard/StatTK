#' Cubic root normalization.
#'
#' CbrtNorm
#' @description Normalize data by cubic root and adding a constant value.
#' @param x.num <numeric>: numerical vector.
#' @param a.num <numeric>: constant value to add before compute cubic root. (Default 0)
#' @return normalized vector.
#' @examples
#' set.seed(542972)
#' x.num <- rnbinom(1000, 1, 0.01)
#' plot(density(x.num))
#' plot(density(CbrtNorm(x.num, abs(min(x.num)))))

CbrtNorm <- function(x.num, a.num=0){(x.num+a.num)**(1/3)}