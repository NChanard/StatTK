#' Pearson median skewness.
#'
#' MedianSkewness
#' @description Pearson median skewness.
#' @param x.num <numeric>: numerical vector.
#' @return skewness value.
#' @examples
#' set.seed(542972)
#' MedianSkewness(rnbinom(10000, 1, 0.01))
#' set.seed(542972)
#' MedianSkewness(rnorm(10000, 1, 0.01))
MedianSkewness <- function(x.num){
    x.num <- x.num[which(is.finite(x.num))]
    return(3*(mean(x.num)-stats::median(x.num))/stats::sd(x.num))
}