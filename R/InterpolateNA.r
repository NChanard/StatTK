#' InterpolateNA
#'
#' Interpolation of NA in a linear or spline way.
#' @param x.num <numeric>: numerical vector with NA
#' @param method.chr <character>: Kind of interpolation "linear" or "spline". (Default "spline")
#' @param ... <...>: suplemantary parameters for smooth.spline function
#' @return ...
#' @examples
#' set.seed(20071)
#' n <- 100
#' y.num <- sin(seq(0, 5*pi, length.out = n)) + rnorm(n=n, mean = 0, sd=0.1)
#' y.num[sample(1:n,round(0.5*n))] <- NA
#' pdf(paste0(getwd(),"/Rplots.pdf"))
#'     plot(y.num)
#'     plot(InterpolateNA(y.num,"spline"))
#'     plot(InterpolateNA(y.num,"linear"))
#' dev.off()
InterpolateNA <- function(x.num, method.chr="spline",...){
    if(method.chr == "spline"){
        smoo <- smooth.spline(seq_along(x.num)[!is.na(x.num)],x.num[!is.na(x.num)],...)
        x.num[is.na(x.num)] <- predict(smoo,seq_along(x.num)[is.na(x.num)])$y
        return(x.num)
    }else if(method.chr == "linear"){
        firstsNa.ndx <- NULL
        if(is.na(x.num[1])){
            firstsNa.ndx <- which(diff(cumsum(is.na(x.num)))==0)[1]
            x.num <- x.num[(firstsNa.ndx+1):length(x.num)]
        }
        lastNa.ndx <- NULL
        if(is.na(tail(x.num,1))){
            lastNa.ndx <- which(diff(cumsum(is.na(rev(x.num))))==0)[1]
            x.num <- x.num[1:(length(x.num)-lastNa.ndx)]
        }
        x.num <- approx(x=x.num, n=length(x.num))$y
        if (!is.null(firstsNa.ndx))    {
            by.num <- mean(diff(x.num))
            firstsNa.ndx <- seq(x.num[1]-(by.num*firstsNa.ndx), x.num[1]-(by.num*(firstsNa.ndx-1)), by=by.num)
            x.num <- c(firstsNa.ndx,x.num)
        }
        if (!is.null(lastNa.ndx)) {
            x.num <- rev(x.num)
            by.num <- mean(diff(x.num))
            lastNa.ndx <- seq(x.num[1]-(by.num*lastNa.ndx), x.num[1]-(by.num*(lastNa.ndx-1)), by=by.num)
            x.num <- c(lastNa.ndx,x.num)
            x.num <- rev(x.num)
        }
        return(x.num)
    }
}

