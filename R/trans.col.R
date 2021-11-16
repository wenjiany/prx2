##' Transparent color
##'
##' copied from the internet
##' 
##' @param col 
##' @param alpha 
##' @return 
##' @author Yang
trans.col <- function(col, alpha=1) {
    do.call(rgb, as.list(c(col2rgb(col)/255, alpha)))
}

