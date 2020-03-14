combine_duplicated_labels <- function(x){
    dl <- duplicated_labels(x)
    l <- labels(x)
    for(i in seq_along(dl)){
        old_val <- dl[[i]]
        new_val <- old_val[1]
        drop_val <- old_val[-1]
        x[x %in% old_val] <- new_val
        drop <- l@values %in% drop_val
        l@.Data <- l@.Data[!drop]
        l@values <- l@values[!drop]
        labels(x) <- l
    }
    x
}


prefix_duplicated_labels <- function(x,
                                      pattern="%d. %s",
                                      ...){
    dl <- duplicated_labels(x)
    if(length(dl)){
        l <- labels(x)
        dedup_lab <- sprintf(pattern,l@values,l@.Data)
        l@.Data <- dedup_lab
        labels(x) <- l
    }
    x
}


postfix_duplicated_labels <- function(x,
                                      pattern="%s (%d)",
                                      ...){
    dl <- duplicated_labels(x)
    l <- labels(x)
    for(i in seq_along(dl)){
        dup_lab <- names(dl)[i]
        dup_val <- dl[[i]]
        ii <- match(dup_val,l@values)
        dedup_lab <- sprintf(pattern,dup_lab,dup_val)
        l@.Data[ii] <- dedup_lab
    }
    labels(x) <- l
    x
}

deduplicate_labels <- function(x,...) UseMethod("deduplicate_labels")
deduplicate_labels.default <- function(x,...) return(x)

deduplicate_labels.item <- function(x,method=c("combine codes",
                                               "prefix values",
                                               "postfix values"),...){
    method <- match.arg(method)
    # browser()
    switch(method,
           "combine codes"=combine_duplicated_labels(x),
           "prefix values"=prefix_duplicated_labels(x,...),
           "postfix values"=postfix_duplicated_labels(x,...))
}

deduplicate_labels.item.list <- function(x,...){
    n <- ncol(x)
    for(i in 1:n){
        x.i <- x[[i]]
        if(length(labels(x.i)) &&
           length(duplicated_labels(x.i))){
            x.i <- deduplicate_labels(x.i,...)
            x@.Data[[i]] <- x.i
        }
    }
    x
}
