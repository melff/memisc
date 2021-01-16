reorder_codes.item <- function(x,from,to){
    stopifnot(length(from)==length(to))
    y <- x
    for(i in 1:length(from)){
        y[x==from[i]] <- to[i]
    }
    if(length(labels(x))){
        l <- labels(x)
        lvy <- lvx <- l@values
        lD <- l@.Data
        for(i in 1:length(from)){
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
