#' Two dimension Gaussian kernel.
#'
#' GaussKernel
#' @description Two dimension Gaussian kernel.
#' @param sd.num <numeric>: standard deviation parameter of the gaussian. (Default 1)
#' @param kernSize.num <numeric>: size of kernel. If null size is 1+4*sd.num. (Default NULL)
#' @param scale.chr <character>: scaling kind of box. If "1" sum of kernel equal 1. If "int" Minimal value of kernel is 1 and all entry are integer. If "none", kernel is not scale. (Default "1")
#' @param by.num <numerical>: step size for compute Gaussian function in two dimension (Default 0.01).
#' @return numerical matrix.
#' @examples
#' GaussKernel(scale.chr=c("none"))
#' GaussKernel(scale.chr=c("1"))
#' sum(GaussKernel(scale.chr=c("1")))
#' GaussKernel(scale.chr=c("int"))
GaussKernel <- function(sd.num=1,kernSize.num=NULL,scale.chr=c("1"), by.num=0.01){
    if(is.null(kernSize.num)){kernSize.num=1+4*sd.num}
    x<-as.vector(scale(seq_len(kernSize.num),scale=FALSE,center=TRUE))
    y<-x
    
    kern.num <- sapply(seq_along(x),function(col){
        sapply(seq_along(y),function(row){
            xInterval.num<-seq((x[col]-0.5),(x[col]+0.5),by=by.num)
            yInterval.num<-seq((y[row]-0.5),(y[row]+0.5),by=by.num)
            kern <- lapply(xInterval.num, function(xi){
                lapply(yInterval.num,function(yj){Gauss(x=xi,y=yj,sd.num=sd.num)})
            }) |>
            unlist() |>
            mean()
            return(kern)
        })
    })
    if(scale.chr == "1" ){
        kern.num  <- kern.num/sum(abs(kern.num))
    }else if(scale.chr == "int" ){
        kern.num  <- ceiling(kern.num/kern.num[1,1])
    }
    return(kern.num)
}