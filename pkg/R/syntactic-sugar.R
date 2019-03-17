"%#%" <- function(x,descr){
    description(x) <- descr
    return(x)
}


"%##%" <- function(x,annot){
    for(n in names(annot)){
        annotation(x)[n] <- as.character(annot[n])
    }
    return(x)
}


"%@%" <- function(x,nm){
    nm <- substitute(nm)
    if(typeof(nm)=="symbol"){
        nm <- deparse(nm)
    } else {
        nm <- eval.parent(nm)
    }
    attr(x,nm)
}

"%@%<-" <- function(x,nm,value){
    nm <- substitute(nm)
    if(typeof(nm)=="symbol"){
        nm <- deparse(nm)
    } else {
        nm <- eval.parent(nm)
    }
    attr(x,nm) <- value
    return(invisible(x))
}
