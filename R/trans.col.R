##' Transparent color
##'
##' copied from the internet
##' 
##' @param col color in string
##' @param alpha transparency 0 to 1 from transparent to opaque
##' @return rgb for transparent color
##' @export
##' @author Yang
##' @examples
##' n <- 100
##' cc <- sample(c('red', 'blue'), n, replace=TRUE)
##' plot(rnorm(n), col=trans.col(cc, 0.6), pch=16)
trans.col <- function(col, alpha=1) {
    sapply(col, function(x) do.call(grDevices::rgb, as.list(c(grDevices::col2rgb(x)/255, alpha))))
}

