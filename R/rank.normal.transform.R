
##' Inverse rank normal transformation for quantative trait
##'
##' Inverse rank normal transofrmation
##' 
##' @title rank.normal.transform
##' @param x numeric vector to be rank-normal-transformed 
##' @return transformed values following normal distribution
##' @export
##' @author Wenjian Yang
##' @examples
##' x <- rexp(100)
##' rx <- rank.normal.transform(x)
##' par(mfrow=c(2, 2))
##' plot(rx, x)
##' hist(rx, main='rank normalized')
##' hist(x, main='original')
##' plot(rx, log(x), xlab='rank normalized')
##' 
rank.normal.transform <- function(x) {
    stats::qnorm((rank(x)-0.5)/sum(!is.na(x)))
}
