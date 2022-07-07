#' GaussBox
#'
#' One dimension Gaussian kernel 
#' @param sd.num <numeric>: standard deviation parameter of the gaussian (Default 1)
#' @param boxSize.num <numeric>: size of kernel. If null size is 1+4*sd.num
#' @param scale.chr <character>: scaling kind of box. If "1" sum of kernel equal 1. If "int" Minimal value of kernel is 1 and all entry are integer. If "none", kernel is not scale (Default "1")
#' @return Numerical vector
#' @examples
#' GaussBox( scale.chr="none")
#' GaussBox( scale.chr="1")
#' sum(GaussBox( scale.chr="1"))
#' GaussBox( scale.chr="int")
GaussBox <- function(sd.num=1, boxSize.num=NULL, scale.chr="1") {
    if(is.null(boxSize.num)){boxSize.num=1+4*sd.num}
    x.num <-as.vector(scale(seq_len(boxSize.num),scale=FALSE,center=TRUE))
    box <- lapply(x.num,function(x){
        xInterval.num<-seq((x-0.5),(x+0.5),by=0.01)
        lapply(xInterval.num, function(xi){Gauss(x=xi,sd.num=sd.num)}) %>% unlist %>% mean %>% return
    }) %>% unlist
    if(scale.chr == "1" ){
        box  %<>% {./sum(abs(.))}
    }else if(scale.chr == "int" ){
        box  %<>% {./.[1]} %>% ceiling
    }
    return(box)
}