#' Normalize data.
#'
#' Normalize
#' @description Normalize data by with square root, cubic root or log2 based on minimal skewness.
#' @param x.num <numeric>: numerical vector
#' @param a.num <numeric>: constant value to add before compute normalisation too avoid negativ number. If null auto dertermine a.num. (Default NULL)
#' @return Normalized vector
#' @examples
#' set.seed(542972)
#' x.num <- rnbinom(1000, 1, 0.01)
#' plot(density(x.num))
#' plot(density(Normalize(x.num)))
Normalize <- function(x.num=NULL, a.num=NULL){
    if(is.null(a.num)){
        if(min(x.num)>0){
            a.num <- 0
        }else if(min(x.num)>=(-1)){
            a.num <- 1
        }else{
            a.num <- abs(min(x.num))
        }
    }
    normMethod.chr <- c(
        "SqrtNorm"=MedianSkewness(SqrtNorm(x.num,a.num)),
        "CbrtNorm"=MedianSkewness(CbrtNorm(x.num,a.num)),
        "Log2Norm"=MedianSkewness(Log2Norm(x.num,a.num))
    ) |> abs() |> which.min() |> names()
    x.num <- eval(parse(text=normMethod.chr))(x.num,a.num) |>
        DevTK::AddAttr(attribute.lst=list(norm=normMethod.chr), overwrite.bln=TRUE)
    return(x.num)
}