
##' retrieve header lines from a text file
##' 
##' read header lines from a text file
##' assuming header starts with '#'
##' 
##' @param filename path to the file
##' @param limit number of lines to check for header, default 5000
##' @param header.char default '#'
##' @return a vector and each line is an alement
##' @author Wenjian Yang
get_file_header <- function(filename, limit=5000, header.char='#') {
    tfile <- file(filename, "r")
    headers <- rep('', limit)
    curr.line <- readLines(tfile, 1)
    i <- 1
    while (substr(curr.line, 0, 1)==header.char) {
        headers[i] <- curr.line
        curr.line <- readLines(tfile, 1)
        i <- i + 1
    }
    close(tfile)
    headers <- headers[headers!='']
    return (headers)
}

