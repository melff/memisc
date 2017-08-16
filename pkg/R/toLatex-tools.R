LaTeXcape <- function(x){
    x <- gsub("$","\\$",x,fixed=TRUE)
    x <- gsub("_","\\_",x,fixed=TRUE)
    x <- gsub("^","\\^",x,fixed=TRUE)
}
