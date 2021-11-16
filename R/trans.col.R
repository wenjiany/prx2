##' Transparent color
##'
##' copied from the internet
##' 
##' @param col color in string
##' @param alpha transparency 0-1
##' @return rgb for transparent color
##' @export
##' @author Yang
trans.col <- function(col, alpha=1) {
    do.call(grDevices::rgb, as.list(c(grDevices::col2rgb(col)/255, alpha)))
}

