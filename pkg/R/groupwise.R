# Some idea, that I did not follow through with ...

eval.by <- function(f,expr) UseMethod("eval.by")
eval.by.default <- function(f,expr){
  parent <- parent.frame()
  expr <- substitute(expr)
  vars <- all.vars(expr)
  vars <- data.frame(sapply(vars,get,parent))
  vars <- split(vars,f)
  res <- lapply(vars,function(v)eval(expr,v,parent))
  unsplit(res,f)
}

# New stuff

getRows_ <- function(i,data) {
    data[i,,drop=FALSE]
}

as_factor <- function(x){
    if(inherits(x,"item.vector")){
        x <- include.missings(x)
        x <- if(is.ordinal(x)) as.ordered(x)
             else if(is.nominal(x)) as.factor(x)
             else as.vector(x)
    }
    if(!is.factor(x)){
        lev <- sort(unique(x),na.last=TRUE)
        i <- match(x,lev)
        x <- factor(i,labels=as.character(lev))
        return(x)
    }
    else {
        # if(any(is.na(x))){
        #     lev <- c(levels(x),"<NA>")
        #     nlev <- length(lev)
        #     x <- as.integer(x)
        #     x[is.na(x)] <- nlev
        #     x <- factor(x,labels=lev)
        # }
        x <- addNA(factor(x),ifany=TRUE)
        return(x)
    }
}

Groups.data.set.formula <- function(data,by,...){
    varn <- all.vars(by)
    vars <- data[varn]
    factors <- lapply(vars,as_factor)
    ii <- 1:nrow(data)
    orig.id <- split(ii,factors)
    structure(tapply(ii,factors,getRows_,data=data),
              orig.id=orig.id,
              row.names=attr(data,"row.names"),
              spec=vars[0,,drop=FALSE],
              class=c(paste("grouped",class(data)[1],sep="."),"grouped.data"))
}

setMethod("Groups",signature(data="data.set",by="formula"),
          Groups.data.set.formula)
setMethod("Groups",signature(data="data.frame",by="formula"),
          Groups.data.set.formula)

eval1call <- function(data,call,envir){
    call$data <- data
    eval(call,envir=envir)
}

within.grouped.data <- function(data,expr,recombine=FALSE,...){
    non.null <- sapply(data,length) > 0
    data_ <- data[non.null]
    call_ <- match.call()
    call_[[1]] <- as.name("within")
    # for(i in 1:length(data_)){
    #     call_$data=data_[[i]]
    #     data_[[i]] <- eval(call_,envir=parent.frame())
    # }
    data_ <- lapply(data_,eval1call,call=call_,envir=parent.frame())
    data[non.null] <- data_
    if(recombine)
        recombine(data)
    else
        data
}

with.grouped.data <- function(data,expr,...){
    non.null <- sapply(data,length) > 0
    res_ <- data[non.null]
    spec_ <- attr(data,"spec")
    call_ <- match.call()
    call_[[1]] <- as.name("with")
    # for(i in 1:length(res_)){
    #     call_$data=res_[[i]]
    #     res_[[i]] <- eval(call_,envir=parent.frame())
    # }
    res_ <- lapply(res_,eval1call,call=call_,envir=parent.frame())
    if(all(non.null)){
        res_len <- sapply(res_,length)
        if(length(unique(res_len))==1) {
            # All results have the same length
            if(length(dim(res_[[1]]))){
                newdim <- c(dim(res_[[1]]),dim(data))
                newdimnames <- c(dimnames(res_[[1]]),dimnames(data))
            } else if(length(res_[[1]]) > 1){
                newdim <- c(length(res_[[1]]),dim(data))
                newdimnames <- c(list(names(res_[[1]])),dimnames(data))
            } else {
                newdim <- c(dim(data))
                newdimnames <- c(dimnames(data))
            }
            if(all(sapply(res_,is.atomic))) {
                data <- unlist(res_)
            } else {
                data <- unlist(res_,recursive=FALSE)
            }
            dim(data) <- newdim
            dimnames(data) <- newdimnames
        } 
    }
    else{
        data[non.null] <- res_
        attr(data,"orig.id") <- NULL
    }
    class(data) <- "grouped.result"
    if(!inherits(spec_,"data.frame")){
        attr(data,"spec") <- spec_
    }
    data
}

print.grouped.result <- function(x,...){
    attr(x,"spec") <- NULL
    class(x) <- NULL
    print.default(x)
}

names.grouped.data <- function(x){
    names(x[[1]])
}


recombine <- function(x,...) UseMethod("recombine")

recombine.grouped.data <- function(x,...) {
    orig.id <- attr(x,"orig.id")
    row.names <- attr(x,"row.names")
    y <- do.call(rbind,x)
    ii <- unlist(orig.id)
    y[ii,] <- y
    structure(y,row.names=row.names)
}

withGroups <- function(data,by,expr,...) {
    data <- Groups(data=data,by=by)
    call_ <- match.call()
    call_[[1]] <- with.grouped.data
    call_$data <- data
    eval(call_,envir=parent.frame())
}
withinGroups <- function(data,by,expr,recombine=TRUE,...) {
    data <- Groups(data=data,by=by)
    call_ <- match.call()
    call_[[1]] <- within.grouped.data
    call_$data <- data
    call_$recombine <- recombine
    eval(call_,envir=parent.frame())
}

