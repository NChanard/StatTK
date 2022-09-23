#' Find the elbow point on a curve.
#'
#' FindElbow
#' @description Find the elbow point on a curve.
#' @param x.num <numeric>: numeric vector of x coordinates of the curve.
#' @param y.num <numeric>: numeric vector of y coordiantes of the curve.
#' @return Numeric vector of elbow coordinates.
#' @examples
#' x.num = 1:10
#' y.num = 1/sqrt(x.num)
#' elbow = FindElbow(x.num, y.num)
#' elbow
#' plot(y.num~x.num,type='l')
#' points(x=elbow[1],y=elbow[2])

FindElbow <- function(x.num, y.num) {
    # Max values to create line
        xMax.num <- max(x.num)
        yxMax.num <- y.num[which.max(x.num)]
        yMax.num <- max(y.num)
        xyMax.num <- x.num[which.max(y.num)]
        max.dtf <- data.frame(x = c(xyMax.num, xMax.num), y = c(yMax.num, yxMax.num))
    # Creating straight line between the max values
        fit.lm <- stats::lm(max.dtf$y ~ max.dtf$x)
    # Distance from point to line
        distances.num <- c()
        for(i in seq_along(x.num)) {
            distances.num <- c(distances.num, abs(stats::coef(fit.lm)[2]*x.num[i] - y.num[i] + stats::coef(fit.lm)[1]) / sqrt(stats::coef(fit.lm)[2]^2 + 1^2))
        }
    # Max distance point
        xMaxDist <- x.num[which.max(distances.num)]
        yMaxDist <- y.num[which.max(distances.num)]
        return(c(xMaxDist, yMaxDist))
}