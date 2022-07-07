#' BoxBlur
#'
#' Apply a blur on matrix with a One dimensional kernel
#' @param mat.mtx <matrix>: Numerical matrix
#' @param box.num <numeric>: the numerical vector for kernel. If Null apply a GaussBox (see 'GaussBox' function)
#' @param boxSize.num <numeric>: If box.num is NULL, size of kernel for 'GaussBox' function.
#' @param sd.num <numeric>: If box.num is NULL,  standard deviation parameter for 'GaussBox' function.
#' @return Blurred matrix
#' @examples
#' set.seed(981643)
#' mat.mtx <- rnorm(10000,50,10)**3 %>% matrix(.,100,100)
#' pdf(file=paste0(getwd(),"/Rplots.pdf"))
#' heatmap(mat.mtx,Rowv=NA,Colv=NA)
#' heatmap(BoxBlur(mat.mtx),Rowv=NA,Colv=NA)
#' dev.off()
BoxBlur <- function(mat.mtx, box.num=NULL, boxSize.num=NULL, sd.num=1){
    if(is.null(box.num)){box.num <- GaussBox(sd=sd.num,scale.chr="1",boxSize.num=boxSize.num)}
    pad.num <- (length(box.num)-1)/2
    mat.mtx <- PadMtx(mat.mtx=mat.mtx, padSize.num=pad.num, value.num=NA, side.chr=c('top','bot','right','left'))
    matVsmth.mtx2 <- sapply(((1+pad.num):(dim(mat.mtx)[2]-pad.num)), function(j){(t(mat.mtx[,(j-pad.num):(j+pad.num)]) * box.num) %>% apply(.,2,Plus)})
    matHsmth.mtx2 <- t(sapply(((1+pad.num):(dim(matVsmth.mtx2)[1]-pad.num)), function(i){(matVsmth.mtx2[(i-pad.num):(i+pad.num),] * box.num) %>% apply(.,2,Plus)%>% t}))
    which.ndx <- which(matHsmth.mtx2==0)
    if(length(which.ndx )){
        matHsmth.mtx2 = Rise0(matHsmth.mtx2,which.ndx=which.ndx)
    }
    return(matHsmth.mtx2)
}
