reverse_codes.item <- function(x){
    y <- x
    if(length(labels(x))){
        l <- labels(x)
        from <- l@values
        to <- rev(from)
        to.l <- l
        to.l@.Data <- rev(l@.Data)
        for(i in seq_along(from)){
            y[x==from[i]] <- to[i]
        }
        labels(y) <- to.l
    } else {
        v <- valid.values(x)
        if(length(v)){
            from <- v@filter
            to <- rev(from)
            for(i in seq_along(from)){
                y[x==from[i]] <- to[i]
            }
        } else stop("No way to determinen how reverse the codes.")
    }
    return(y)
}



setGeneric("reverse_codes",
           function(x)standardGeneric("reverse_codes"))
setMethod("reverse_codes","item.vector",reverse_codes.item)

reverse_codes.factor <- function(x){
    y <- factor(x,levels=rev(levels(x)))
    return(y)
}

setMethod("reverse_codes","item.factor",reverse_codes.factor)
