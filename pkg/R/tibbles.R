setGeneric("prep_for_tibble",function(x,...)standardGeneric("prep_for_tibble"))
setMethod("prep_for_tibble",signature(x="item.vector"),function(x,...)
    prep_for_tibble_item_vector(x))

prep_for_tibble_item_vector <- function(x,...){
    y <- if(inherits(x,"character.item")) as.character(x)
         else if(is.ordinal(x)) as.ordered(x)
         else if(is.nominal(x)) as.factor(x)
         else as.vector(x)
    d <- description(x)
    if(length(d))
        attr(y,"label") <- d
    return(y)
}

as_tibble.data.set <- function(x, ...){
    y <- lapply(x@.Data,
                prep_for_tibble_item_vector)
    names(y) <- names(x)
    attr(y,"row.names") <- x@row_names
    class(y) <- c("tbl_df","tbl","data.frame")
    return(y)
}

setOldClass("haven_labelled")
setMethod("as.item",signature(x="haven_labelled"),function(x,...){
    annotation <- c(description=attr(x,"label"))
    labels <- attr(x,"labels")
    class(x) <- NULL
    as.item(x,labels=labels,
            annotation=annotation)
})

setOldClass("haven_labelled_spss")
setMethod("as.item",signature(x="haven_labelled_spss"),function(x,...){
    annotation <- c(description=attr(x,"label"))
    labels <- attr(x,"labels")
    mis_range <- attr(x,"na_range")
    mis_values <- attr(x,"na_values")
    class(x) <- NULL
    value_filter <- new("missing.values",
                        filter=mis_values,
                        range=mis_range)
    as.item(x,labels=labels,
            annotation=annotation,
            value.filter=value_filter)
})

setGeneric("as_haven",function(x,...)standardGeneric("as_haven"))
setMethod("as_haven",signature(x="data.set"),function(x,user_na=FALSE,...){
    y <- lapply(x@.Data,as_haven,user_na=user_na,...)
    names(y) <- names(x)
    attr(y,"row.names") <- x@row_names
    class(y) <- c("tbl_df","tbl","data.frame")
    return(y)
})
setMethod("as_haven",signature(x="item.vector"),function(x,user_na=FALSE,...){
    y <- x@.Data
    attr(y,"label") <- description(x)
    attr(y,"labels") <- as.vector(labels(x))
    ms <- missing.values(x)
    if(user_na && length(ms)){
        attr(y,"na_values") <- ms@filter
        attr(y,"na_range") <- ms@range
        class(y) <- "haven_labelled_spss"
    } else {
        ism <- is.missing(x)
        y[ism] <- NA
        class(y) <- "haven_labelled"
    }
    return(y)
})
