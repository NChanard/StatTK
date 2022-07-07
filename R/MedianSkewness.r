#' MedianSkewness
#'
#' Pearson Median Skewness
#' @param x.num <numeric>: Numerical vector
#' @return Skewness value
#' @examples
#' set.seed(542972)
#' MedianSkewness(rnbinom(10000, 1, 0.01))
#' set.seed(542972)
#' MedianSkewness(rnorm(10000, 1, 0.01))
MedianSkewness <- function(x.num){
    x.num %<>% .[which(is.finite(.))]
    return(3*(mean(x.num)-median(x.num))/sd(x.num))
}