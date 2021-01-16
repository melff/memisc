trim_labels.item <- function(x,...){
    l <- labels(x)
    lData <- l@.Data
    l@.Data <- gsub("[-+]?[0-9]+[.] ","",lData)
    labels(x) <- l
    x
}

trim_labels.data.set <- function(x,...){
    for(i in seq_along(x)){
        x@.Data[[i]] <- trim_labels.item(x@.Data[[i]])
    }
    x
}

setGeneric("trim_labels",
           function(x,...)standardGeneric("trim_labels"))

setMethod("trim_labels","item.vector",trim_labels.item)
setMethod("trim_labels","data.set",trim_labels.data.set)
