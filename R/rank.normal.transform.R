##' .. content for \description{} (no empty lines) ..
##' Inversed rank normal transformation for quantative trait
##' .. content for \details{} ..
##' @title rank.normal.transform
##' @param x 
##' @return transformed values following normal distribution
##' @author Wenjian Yang
rank.normal.transform <- function(x) {
    qnorm((rank(x)-0.5)/sum(!is.na(x)))
}
