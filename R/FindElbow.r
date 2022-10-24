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
        xMin.num <- min(x.num)
        yxMin.num <- y.num[which.min(x.num)]
        max.dtf <- data.frame(x = c(xMin.num, xMax.num), y = c(yxMin.num, yxMax.num))
    # Creating straight line between the max values
        fit.lm <- stats::lm(max.dtf$y ~ max.dtf$x)
    # Distance from point to line
        a = stats::coef(fit.lm)[2]
        angle_pente = 90-tan(a) * pi /180
        b = stats::coef(fit.lm)[1]
        i = 1
        max_delta = -Inf
        while(i <= length(x.num)){
            vertical_distances.num = abs(a*x.num[i]+b - y.num[i])
            perpendicular_distance.num = sin(angle_pente)*vertical_distances.num
            delta = vertical_distances.num - perpendicular_distance.num
            if(delta>max_delta){
                max_delta <- delta
                i_max <- i
            }else if (i_max+3<= i ){
                return(c(x.num[which.max(i_max)], y.num[which.max(i_max)]))
            }
            i <- i+1
        }
        return(NULL)        
}