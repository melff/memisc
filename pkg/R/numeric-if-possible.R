numericIfPossible <- function(x){
    if(is.atomic(x)) return(.numericIfPossible(x))
    else {
        res <- lapply(x,.numericIfPossible)
        attributes(res) <- attributes(x)
        return(res)
    }
}

.numericIfPossible <- function(x){
    if(is.numeric(x)) return(x)
    else if(is.character(x)) return(.Call("numeric_if_possible", x))
    else if(is.factor(x)) {
        levels <- .Call("numeric_if_possible",levels(x))
        if(is.numeric(levels)){
            return(levels[as.numeric(x)])
        } else return(x)
    }
    else return(x)
}
