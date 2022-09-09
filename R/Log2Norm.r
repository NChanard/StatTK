#' Log2 normalization.
#'
#' Log2Norm
#' @description Normalize data by logarithme (base 2) and adding a constant value.
#' @param x.num <numeric>: numerical vector.
#' @param a.num <numeric>: constant value to add before compute cubic root. (Default 0)
#' @return normalized vector.
#' @examples
#' set.seed(542972)
#' x.num <- rnbinom(1000, 1, 0.01)
#' plot(density(x.num))
#' plot(density(Log2Norm(x.num, abs(min(x.num)))))
Log2Norm <- function(x.num, a.num=0){log2(x.num+a.num)}