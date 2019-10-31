## A variant of 'list' that names its elements just like
# 'data.frame' does:

List <- function(...){
    args <- list(...)
    argnames <- sapply(substitute(c(...))[-1],as.character)
    argtags <- names(args)
    if(length(argtags)){
        nz <- nzchar(argtags)
        argnames[nz] <- argtags[nz]
    }
    names(args) <- argnames
    args
}

