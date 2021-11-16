##' extract odds ratio/hazards ratio from glm or coxph model
##'
##' Compute Odds Ratio/Hazards ratio based on estimate and standard error
##' 
##' @param m model
##' @return a matrix with est, 95%CI and pvalue
##' @export
##' @author Wenjian Yang
get_odds_ratio <- function(m) {
    curr.coef <- summary(m)$coefficients[-1,, drop=FALSE]
    odds.ratio <- exp(rep(curr.coef[,1, drop=FALSE],3) + t(stats::qnorm(c(0.5, 0.025, 0.975)) %*% t( curr.coef[,2, drop=FALSE])))
    odds.ratio <- cbind(odds.ratio, curr.coef[, ncol(curr.coef)])
    colnames(odds.ratio) <- c('est', 'ci.95.low', 'ci.95.high', 'pvalue')
    return(odds.ratio)
}

##' Compute Confidence intervals from estimates and p-values
##'
##' Compute Confidence intervals from estimates and p-values based on normal distribution
##' 
##' @param ests a vector of coefficients of estimate
##' @param pvalues a vector of p-values
##' @param ci confidence interval, default 0.95
##' @param exponential whether take the exponential transformation for the coef, default FALSE
##' @return a matrix with est, 95%CI and pvalue
##' @export
##' @author Wenjian Yang
get_conf_int <- function(ests, pvalues, ci=0.95, exponential=FALSE) {
    stderrs <- ests/stats::qnorm(1 - pvalues/2)
    ci_stat <- stats::qnorm(1 - (1-ci)/2)
    ci.low <- ests - stderrs * ci_stat
    ci.high <- ests + stderrs * ci_stat
    if (exponential) {
        ests <- exp(ests)
        ci.low <- exp(ci.low)
        ci.high <- exp(ci.high)
    }
    results <- cbind(ests, ci.low, ci.high, pvalues)
    colnames(results) <- c('est', 'ci.95.low', 'ci.95.high', 'pvalue')
    return(results)
}

