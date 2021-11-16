##' Plot boxplot together with stripchart dotplot.
##' 
##' @param y values
##' @param x groups
##' @param ... will be parsed to boxplot and stripchart
##' @return
##' @export
##' @author Wenjian Yang
boxstripchart <- function(y, x, ...) {

  user.args <- list(...)
  
  x.num <- as.numeric(factor(x))
  x.lim <- c(min(x.num, na.rm=TRUE) - 0.5, max(x.num, na.rm=TRUE) + 0.5)
  
  y.sorted <- unique(sort(y))
  pstep <- stats::quantile(y.sorted[-1] - y.sorted[-length(y.sorted)], 0.1)
  
  ## get min/max
  pp <- boxplot(y ~ x, plot=FALSE)
  pmin <- min(pp$stats) - pstep
  pmax <- max(pp$stats) + pstep

  boxplot.args <- list(formula=y ~ x, outline=FALSE, ylim=c(pmin, pmax))
  boxplot.args <- utils::modifyList(boxplot.args, user.args)

  do.call('boxplot', boxplot.args)

  strip.args <- list(x=y ~ x, vert=T, add=T, col="red", pch=16, method="jitter", xlim=x.lim)
  strip.args <- utils::modifyList(strip.args, user.args)
  do.call('stripchart', strip.args)
}

boxplot.prx <- function(formula, xaxis.at=1:3, xaxis.label=c('AA', 'AB', 'BB'), point.col='red', pch=16, alpha=0.5, jitter.factor=0.2, ...) {
    boxplot(formula, axes=FALSE, axes=FALSE, ...)
    box(bty='l')
    axis(2); axis(1, at=xaxis.at, label=xaxis.label)

    yx <- eval(attributes(terms(formula))$variables, parent.frame())
    
    points(jitter(yx[[2]], factor=jitter.factor)+1, yx[[1]], col=trans.col(point.col, alpha), pch=pch)
}

##' Plot boxplot together with violin dots
##' 
##' @param y variable to plot
##' @param x group variable
##' @param srt angle of x-axis annotation 
##' @param xtext.adj placement of x-axis nnotation
##' @param box.more.args list of args for boxplot 
##' @param dots.more.args list of args for dots, col/pch are by group 
##' @param cex.axis size of axis labels
##' @param ... additional argments to parse down to boxplot and viopoints
##' @return none
##' @export
##' @author Wenjian Yang
boxviolin <- function (y, x, srt = 0, xtext.adj = NULL, box.more.args=list(), dots.more.args=list(), cex.axis=0.8, ...) 
{
  if (!requireNamespace("viopoints", quietly = TRUE)) {
    stop("Package \"viopoints\" needed for this function to work. Please install.", 
         call. = FALSE)
  }
  
  user.args <- list(...)
  missing <- is.na(y) | is.na(x)
  x <- x[!missing]
  y <- y[!missing]
  
  x.factor <- factor(x)
  x.count <- table(x.factor)
  x.levels <- levels(x.factor)
  x.num <- as.numeric(factor(x))
  
  x.lim <- c(min(x.num, na.rm = TRUE) - 0.5, max(x.num, na.rm = TRUE) + 
               0.5)
  y.sorted <- unique(sort(y))
  pstep <- stats::quantile(y.sorted[-1] - y.sorted[-length(y.sorted)], 
                    0.1)
  if (is.na(pstep)) {
    pstep <- 0
  }
  
  pp <- boxplot(y ~ x, plot = FALSE)
  pmin <- min(pp$stats) - pstep
  pmax <- max(pp$stats) + pstep

  if (pmin < 0) {pmin <- min(pp$stats)}
  
  boxplot.args <- list(formula = y ~ x, outline = FALSE, ylim = c(pmin, 
                                                                  pmax))
  boxplot.args <- utils::modifyList(boxplot.args, user.args)
  boxplot.args <- utils::modifyList(boxplot.args, box.more.args)
  
  boxplot.args$axes = FALSE
  do.call("boxplot", boxplot.args)
  box(bty = "L")
  axis(2, at=c(0.05, 0.1, 0.5, 1, 5, 10, 50), labels=c(0.05, 0.10, 0.50, 1.0, 5.0, 10, 50))
  if (is.null(xtext.adj)) {
    axis(1, at = 1:nlevels(x.factor), label = paste0(levels(x.factor), 
                                                     "\n(n=", x.count, ")"), srt = srt, cex.axis=cex.axis)
  }
  else {
    axis(1, at = 1:nlevels(x.factor), label = FALSE)
    text(x = 1:nlevels(x.factor), y = xtext.adj, labels = paste0(levels(x.factor), 
                                                                 "\n(n=", x.count, ")"), srt = srt, adj = 1, xpd = TRUE, cex.axis=cex.axis)
  }
  vio.args <- list(y, groups = x, col = "red", add = TRUE, 
                   pch = 16, method = "violin")
  vio.args <- utils::modifyList(vio.args, user.args)
  vio.args <- utils::modifyList(vio.args, dots.more.args)
  do.call("viopoints::viopoints", vio.args)
  
  boxplot.args$add <- TRUE
  boxplot.args$col <- NA
  do.call("boxplot", boxplot.args)
}

