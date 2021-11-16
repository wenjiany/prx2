##' Concise print a table
##'
##' create a string representing a table,
##' with default row_separator "," and default column_separator "|"
##' @param tt table
##' @param row.sep row_separator, default ","
##' @param col.sep column_separator, default "|"
##' @return a string
##' @export
##' @author Wenjian Yang
stringify.table <- function(tt, row.sep=",", col.sep="|") {
    if (!is.matrix(tt)) {
        curr.colnames <- names(tt)
        tt <- matrix(tt, nrow=1, byrow=TRUE)
    } else {        
        curr.colnames <- colnames(tt)
    }
    row.string <- apply(tt, 2, function(x) paste(x, collapse=row.sep))
    paste(paste0(curr.colnames, ":", row.string), collapse=col.sep)
}

##' Format pvalues in a table
##'
##' Look for columns that representing p-values, range 0 to 1;
##' if p-value is less than 1, signif(pvalue, digits);
##' else, round(pvalue, digits).
##' 
##' @param x table
##' @param digits digits to keep for p-values
##' @return a table with proper digits for p-values
##' @export
##' @author Wenjian Yang
format_pvalue_table <- function(x, digits=3) {  
  num.cols <- which(sapply(x, is.numeric))
  for (i in num.cols) {
    curr.values <- x[[i]]
    curr.min <- min(curr.values, na.rm=TRUE)
    curr.max <- max(curr.values, na.rm=TRUE)
    like.pvalue <- curr.min >= 0 & curr.max <= 1
    if (like.pvalue) {
      x[[i]] <- as.character(ifelse(curr.values < 0.01, signif(curr.values, digits), round(curr.values, digits))) 
    } else {
      x[[i]] <- round(curr.values, digits)
    }
  }
  return(x)
}

