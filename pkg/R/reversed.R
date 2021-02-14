reversed.item <- function(x){
    y <- x
    if(length(labels(x))){
        l <- labels(x)
        vals <- vals.orig <- l@values
        labs <- l@.Data
        vf <- value.filter(x)
        from <- vals[is.valid2(vals,vf)]
        to <- rev(from)
        for(i in seq_along(from)){
            y[x==from[i]] <- to[i]
            vals[vals.orig==from[i]] <- to[i]
        }
        to.l <- structure(vals,names=labs)
        ii <- order(to.l)
        to.l <- to.l[ii]
        labels(y) <- to.l
    } else {
        v <- valid.values(x)
        if(length(v)){
            from <- v@filter
            to <- rev(from)
            for(i in seq_along(from)){
                y[x==from[i]] <- to[i]
            }
        } else stop("No way to determine how reverse the codes.")
    }
    return(y)
}




reversed.factor <- function(x){
    y <- factor(x,levels=rev(levels(x)))
    return(y)
}

setGeneric("reversed",
           function(x)standardGeneric("reversed"))
setMethod("reversed","item.vector",reversed.item)
setMethod("reversed","factor",reversed.factor)
