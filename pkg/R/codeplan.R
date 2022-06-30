setMethod("codeplan",
          signature(x="ANY"),function(x) NULL)

setMethod("codeplan",
          signature(x="item.list"),
          function(x){
    dat <- x@.Data
    n <- x@names
    cp <- lapply(dat,codeplan1)
    names(cp) <- n
    structure(cp,
              class="codeplan")
})

setMethod("codeplan",
          signature(x="item"),
          function(x){
              cp <- codeplan1(x)
              structure(list(cp),class="codeplan")
})

codeplan1 <- function(x) {
    l <- list(
        annotation=depr_annot(x),
        labels=depr_labels(x),
        value.filter=depr_vfilter(x),
        mode=mode(x),
        measurement=measurement(x)
    )
    l[sapply(l,length)>0]
}

print.codeplan <- function(x,...){
    cat("\n",as.yaml(unclass(x)),"\n",sep="")
}

any.nzchar <- function(x) any(nzchar(x))


formatw <- function(x,width){
    l <- nchar(x)
    to.abbr <- l > width
    x[to.abbr] <- paste0(substr(x[to.abbr],1,width-3),"...")
    format(x)
}

depr_annot <- function(x){
    a <- annotation(x)
    a <- structure(a@.Data, names=names(a))
    if(length(a)) as.list(a)
    else NULL
}

depr_labels <- function(x){
    l <- labels(x)
    if(length(l))
        structure(as.list(l@values),names=l@.Data)
    else NULL
}

depr_vfilter <- function(x){
    v <- value.filter(x)
    if(length(v)){
        cl <- as.character(class(v))
        if(cl == "missing.values"){
            res <- list(class=cl,
                        range=v@range,
                        values=v@filter)
        }
        else if(cl == "valid.values"){
            res <- list(class=cl,
                        values=v@filter)
        }
        else if(cl == "valid.range"){
            res <- list(class=cl,
                        range=v@filter)
        }
        else res <- NULL
        res <- res[sapply(res,length)>0]
    }
    else NULL
}

setMethod("setCodeplan",signature(x="data.frame",value="codeplan"),
function(x,value){
    x <- as.data.set(x)
    setCodeplan(x,value)
})

setMethod("setCodeplan",signature(x="data.frame",value="NULL"),
function(x,value) x)

setMethod("setCodeplan",signature(x="data.set",value="codeplan"),
function(x,value){
    n <- intersect(names(x),names(value))
    if(length(n))
        for(nn in n){
            x[[nn]] <- setCodeplan1(x[[nn]],value[[nn]])
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
    setCodeplan1(x,value[[1]])
})

setMethod("setCodeplan",signature(x="atomic",value="codeplan"),
function(x,value){
    setCodeplan1(x,value[[1]])
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
    if(length(l)){
        l <- unlist(l)
        labels(x) <- l
    }
    m <- val$measurement
    measurement(x) <- m
    a <- val$annotation
    if(length(a)){
        annotation(x) <- unlist(a)
    }
    vf <- val$value.filter
    if(length(vf)){
        cl <- vf$class
        if(cl=="missing.values")
            vf <- new(cl,
                      filter=vf$values,
                      range=vf$range
                      )
        else if(cl=="valid.values")
            vf <- new(cl,filter=vf$values)
        else if(cl=="valid.range")
            vf <- new(cl,filter=vf$range)
        x@value.filter <- vf
    }
    x
}

write_codeplan <- function(x,filename,
                           type=NULL,
                           pretty=FALSE){
    if(!length(type)){
        if(endsWith(filename,".yaml") ||
           endsWith(filename,".yml"))
            type <- "yaml"
        else if(endsWith(filename,".json"))
            type <- "json"
    }

    if(type=="yaml"){
        write_yaml(unclass(x),file=filename)
    }
    else if(type=="json"){
        write_json(unclass(x),path=filename,
                   auto_unbox=TRUE,pretty=pretty)
    }
    else stop(sprintf("File type %s not supported (yet)",type))
}

read_codeplan <- function(filename,
                          type=NULL){
    if(!length(type)){
        if(endsWith(filename,".yaml") ||
           endsWith(filename,".yml"))
            type <- "yaml"
        else if(endsWith(filename,".json"))
            type <- "json"
    }

    if(type=="yaml"){
        structure(read_yaml(file=filename),class="codeplan")
    }
    else if(type=="json"){
        structure(read_json(path=filename, simplifyVector = TRUE),
                  class="codeplan")
    }
    else stop(sprintf("File type %s not supported (yet)",type))
}
