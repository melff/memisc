setMethod("codeplan",
          signature(x="ANY"),function(x) NULL)

setMethod("codeplan",
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
    structure(df,class=c("codeplan","data.frame"))
})

setMethod("codeplan",
          signature(x="item"),
          function(x){
    df <- data.frame(
               description=depr_descr(x),
               annotation=depr_annot(x),
               labels=depr_labels(x),
               value.filter=depr_vfilter(x),
               mode=mode(x),
               measurement=measurement(x),
               stringsAsFactors=FALSE)
    structure(df,class=c("codeplan","data.frame"))
})


print.codeplan <- function(x,...,width=getOption("width")%/%5){
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
        if(cl == "missing.values"){
            cl <- "missing"
            v <- list(range=v@range,
                      values=v@filter)
        }
        else if(cl == "valid.values"){
            cl <- "valid"
            v <- list(values=v@filter)
        }
        else if(cl == "valid.range"){
            cl <- "valid"
            v <- list(range=v@filter)
        }
        paste(cl,"=",
              paste(deparse(v),collapse=" "))
    }
    else ""
}

setMethod("setCodeplan",signature(x="data.frame",value="codeplan"),
function(x,value){
    x <- as.data.set(x)
    setCodeplan(x,value)
})

setMethod("setCodeplan",signature(x="data.set",value="codeplan"),
function(x,value){
    vn <- value$name
    n <- intersect(names(x),vn)
    if(length(n))
        for(nn in n){
            i <- match(nn,vn)              
            x[[nn]] <- setCodeplan1(x[[nn]],value[i,])
        }
    x
})

setMethod("setCodeplan",signature(x="data.set",value="NULL"),
function(x,value){
    y <- as.list(x)
    y <- lapply(y,setCodeplan,value=NULL)
    as.data.frame(y,row.names=row.names(x))
})

setMethod("setCodeplan",signature(x="item",value="codeplan"),
function(x,value){
    setCodeplan1(x,value[1,])
})

setMethod("setCodeplan",signature(x="atomic",value="codeplan"),
function(x,value){
    setCodeplan1(x,value[1,])
})

setMethod("setCodeplan",signature(x="item",value="NULL"),
function(x,value){
    x@.Data
})

setMethod("setCodeplan",signature(x="atomic",value="NULL"),
function(x,value){
    x@.Data
})



"codeplan<-" <- function(x,value) {
    x <- setCodeplan(x,value)
    invisible(x)
}

setCodeplan1 <- function(x,val){

    if(is.null(x)){
        x <- vector(mode=val$mode)
    }
    else if(mode(x) != val$mode)
        stop(sprintf("mode conflict: '%s' != '%s'",
                     mode(x),val$mode))
    l <- val$labels
    if(nzchar(l)){
        l <- eval(parse(text=l))
        labels(x) <- l
    }
    m <- val$measurement
    measurement(x) <- m
    a <- val$annotation
    if(nzchar(a)){
        annotation(x) <- eval(parse(text=a))
    }
    d <- val$description
    if(nzchar(d)){
        description(x) <- d
    }
    vf <- val$value.filter
    if(nzchar(vf)){
        vf <- paste0("list(",vf,")")
        vf <- eval(parse(text=vf))
        cl <- names(vf)
        vf <- vf[[1]]
        if(cl=="missing")
            vf <- new("missing.values",
                      filter=vf$values,
                      range=vf$range
                      )
        else if(cl=="valid"){
            if(length(vf$values))
                vf <- new("valid.values",filter=vf$values)
            else if(length(vf$range))
                vf <- new("valid.range",filter=vf$range)
        }
        x@value.filter <- vf
    }
    x
}
