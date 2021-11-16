
##' Inverse rank normal transformation for quantative trait
##'
##' Inverse rank normal transofrmation
##' 
##' @title rank.normal.transform
##' @param x numeric vector to be rank-normal-transformed 
##' @return transformed values following normal distribution
##' @export
##' @author Wenjian Yang
rank.normal.transform <- function(x) {
    stats::qnorm((rank(x)-0.5)/sum(!is.na(x)))
}
