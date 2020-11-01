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

sanitize_labels <- function(labels){
    labn <- names(labels)
    if(length(unique(labels)) < length(labn)){
        dup <- duplicated(labels)
        warning(sprintf("Dropped non-unique label(s) %s",
                        paste(paste(labn[dup],"=",labels[dup]),collapse=", ")),
                call.=FALSE,immediate.=TRUE)
        labels <- labels[!dup]
    }
    labels
}

setOldClass("labelled")
setMethod("as.item",signature(x="labelled"),function(x,...){
    annotation <- c(description=attr(x,"label",exact=TRUE))
    labels <- attr(x,"labels",exact=TRUE)
    labels <- sanitize_labels(labels)
    attributes(x) <- NULL
    as.item(x,labels=labels,
            annotation=annotation)
})
setMethod("codebookEntry","labelled",function(x){
    x <- as.item(x)
    annotation <- annotation(x)
    spec <- c(
        "Storage mode:"=storage.mode(x),
        "Measurement:"="undefined"
    )
    new("codebookEntry",
        spec = spec,
        stats = codebookStatsCateg(x),
        annotation = annotation
        )
})

# if(!requireNamespace("haven",quietly = TRUE))
    setOldClass("haven_labelled")
setMethod("as.item",signature(x="haven_labelled"),function(x,...){
    annotation <- c(description=attr(x,"label",exact=TRUE))
    labels <- attr(x,"labels",exact=TRUE)
    labels <- sanitize_labels(labels)
    attributes(x) <- NULL
    as.item(x,labels=labels,
            annotation=annotation)
})
setMethod("codebookEntry","haven_labelled",function(x){
    x <- as.item(x)
    annotation <- annotation(x)
    spec <- c(
        "Storage mode:"=storage.mode(x),
        "Measurement:"="undefined"
    )
    new("codebookEntry",
        spec = spec,
        stats = codebookStatsCateg(x),
        annotation = annotation
        )
})



# if(!requireNamespace("haven",quietly = TRUE))
    setOldClass("haven_labelled_spss")
setMethod("as.item",signature(x="haven_labelled_spss"),function(x,...){
    annotation <- c(description=attr(x,"label",exact=TRUE))
    labels <- attr(x,"labels",exact=TRUE)
    labels <- sanitize_labels(labels)
    mis_range <- attr(x,"na_range")
    mis_values <- attr(x,"na_values")
    attributes(x) <- NULL
    value_filter <- new("missing.values",
                        filter=mis_values,
                        range=mis_range)
    as.item(x,labels=labels,
            annotation=annotation,
            value.filter=value_filter)
})
setMethod("codebookEntry","haven_labelled_spss",function(x){
    x <- as.item(x)
    annotation <- annotation(x)
    filter <- x@value.filter
    spec <- c(
        "Storage mode:"=storage.mode(x),
        "Measurement:"="undefined"
    )
    if(length(filter)) spec <- c(spec,
                                 switch(class(filter),
                                        missing.values = c("Missing values:" = format(filter)),
                                        valid.values   = c("Valid values:"   = format(filter)),
                                        valid.range    = c("Valid range:"    = format(filter))
                                        ))
    new("codebookEntry",
        spec = spec,
        stats = codebookStatsCateg(x),
        annotation = annotation
        )
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

if(!requireNamespace("tibble",quietly = TRUE)) setOldClass("tbl_df")
setMethod("as.data.set","tbl_df",function(x,row.names=NULL,...){
  class(x) <- "data.frame"
  if(length(row.names)){
    if(length(row.names)!=nrow(x)) stop("row.names argument has wrong length")
    attr(x,"row.names") <- row.names
  }
  else
    attr(x,"row.names") <- seq_len(nrow(x))
  new("data.set",x)
})
