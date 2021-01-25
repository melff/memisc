reorder_codes.item <- function(x,from,to){
    stopifnot(length(from)==length(to))
    y <- x
    for(i in seq_along(from)){
        y[x==from[i]] <- to[i]
    }
    if(length(labels(x))){
        l <- labels(x)
        lvy <- lvx <- l@values
        lD <- l@.Data
        for(i in seq_along(from)){
            lvy[lvx==from[i]] <- to[i]
        }
        i <- order(lvy)
        l@values <- lvy[i]
        l@.Data <- lD[i]
        labels(y) <- l
    }
    return(y)
}



setGeneric("reorder_codes",
           function(x,from,to)standardGeneric("reorder_codes"))
setMethod("reorder_codes","item.vector",reorder_codes.item)

reverse.item <- function(x){
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



setGeneric("reverse",
           function(x)standardGeneric("reverse"))
setMethod("reverse","item.vector",reverse.item)

reverse.factor <- function(x){
    y <- factor(x,levels=rev(levels(x)))
    return(y)
}

setMethod("reverse","item.factor",reverse.factor)
