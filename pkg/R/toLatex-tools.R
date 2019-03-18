LaTeXcape <- function(x){
    x <- gsub("$","\\$",x,fixed=TRUE)
    x <- gsub("_","\\_",x,fixed=TRUE)
    x <- gsub("^","\\^",x,fixed=TRUE)
}

checkLaTeXcape <- function(x,where){
    do.warn <- any(grepl("$",x,fixed=TRUE))
    do.warn <- do.warn || any(grepl("_",x,fixed=TRUE))
    do.warn <- do.warn || any(grepl("^",x,fixed=TRUE))
    if(do.warn)
        warning("Unescaped TeX special in ",where)
}
