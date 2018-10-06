setMethod("describe_structure",
          signature(x="item.list"),
          function(x){
    dat <- x@.Data
    n <- x@names
    df <- data.frame(name=n,
               description=sapply(dat,depr_descr),
               annotation=sapply(dat,depr_annot),
               labels=sapply(dat,depr_labels),
               value.filter=sapply(dat,depr_vfilter),
               mode=sapply(dat,mode),
               measurement=sapply(dat,measurement),
               stringsAsFactors=FALSE)
    structure(df,class=c("ds_des_df","data.frame"))
})

print.ds_des_df <- function(x,...,width=getOption("width")%/%5){
    for(i in 1:length(x))
        x[[i]] <- formatw(x[[i]],width)
    to.keep <- sapply(x,any.nzchar)
    print.data.frame(x[to.keep],row.names=FALSE)
}

any.nzchar <- function(x) any(nzchar(x))


formatw <- function(x,width){
    l <- nchar(x)
    to.abbr <- l > width
    x[to.abbr] <- paste0(substr(x[to.abbr],1,width-3),"...")
    format(x)
}

depr_descr <- function(x){
    d <- description(x)
    if(length(d))d else ""
}

depr_annot <- function(x){
    a <- annotation(x)
    a <- structure(a@.Data, names=names(a))
    i <- match("description",names(a))
    a <- a[-i]
    if(length(a))
        paste(deparse(a),collapse=" ")
    else ""
}

depr_labels <- function(x){
    l <- labels(x)
    if(length(l)){
        l <- structure(l@values,names=l@.Data)
        paste(deparse(l),collapse=" ")
    }
    else ""
}

depr_vfilter <- function(x){
    v <- value.filter(x)
    if(length(v)){
        cl <- as.character(class(v))
        if(cl == "missing.values")
            cl <- "missing"
        else if(cl %in% c("valid.values","valid.range"))
            cl <- "valid"
        v <- list(range=v@range,
                  values=v@filter)
        paste(cl,"=",
              paste(deparse(v),collapse=" "))
    }
    else ""
}
