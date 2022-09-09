#' Find threshold for outliers triming based on inter quartile range.
#'
#' IqrThreshold
#' @description Find threshold for outliers triming based on inter quartile range.
#' @param x.num <numeric>: numeric vector.
#' @param iqrFactor.num <numeric>: inter quartile range factor. (Default 1.5)
#' @param bounds.chr <character>: bounds to return, "lower", "upper" or "both". (Default "both")
#' @return numerical vector of minmal and/or maximal thresholds values for outliers triming.
#' @examples
#' set.seed(1111)
#' x.num <- rnorm(1000)
#' x.num <- sort(x.num)
#' x.num
#' IqrThreshold(x.num, bounds.chr="lower")
#' IqrThreshold(x.num, bounds.chr="both")
#' IqrThreshold(x.num, bounds.chr="upper")

IqrThreshold <- function(x.num=NULL, iqrFactor.num=1.5, bounds.chr="both"){
    Q1.num <- stats::quantile(x.num,0.25)
    Q3.num <- stats::quantile(x.num,0.75)
    interQ.num <- Q3.num-Q1.num
    upper.num <- Q3.num+(iqrFactor.num*interQ.num)
    lower.num <- Q1.num-(iqrFactor.num*interQ.num)
    dplyr::case_when(
        bounds.chr =="both" ~ c(lower.num,upper.num),
        bounds.chr =="upper" ~ c(NA,upper.num),
        bounds.chr =="lower" ~ c(lower.num,NA)
    ) %>% return(.data)
}