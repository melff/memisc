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
    ii <- tapply(ii,factors,I)
    extra.attr <- list(groups=ii,
                       spec=vars[0,,drop=FALSE],
                       class=c(paste("grouped",class(data)[1],sep="."),"grouped.data",class(data)))
    attributes(data) <- c(attributes(data),extra.attr)
    data
}

setMethod("Groups",signature(data="data.set",by="formula"),
          Groups.data.set.formula)
setMethod("Groups",signature(data="data.frame",by="formula"),
          Groups.data.set.formula)

eval1call <- function(data,call,envir){
    call$data <- data
    eval(call,envir=envir)
}

mklen <- function(x,n){
    y <- vector(length=n,mode=mode(x))
    y[] <- x
    return(y)
}

within1.grouped.data.frame <- function(ii,data,expr,parent=parent.frame(),...){
    if(!length(ii)) return(NULL)
    e <- evalq(environment(),data[ii,,drop=FALSE],parent)
    eval(expr,e)
    l <- as.list(e,all.names=TRUE)
    # l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    n <- length(ii)
    ll <- vapply(l,length,0,USE.NAMES=FALSE)
    ll1 <- which(ll==1)
    l[ll1] <- lapply(l[ll1],mklen,n)
    wrongl <- which(ll != n)
    l[wrongl] <- NULL
    return(l)                
}

rbind1col <- function(i,mat){
    do.call(c,mat[,i])
}

reorder1 <- function(x,ii){
    x[ii] <- x
    return(x)
}

within.grouped.data.frame <- function(data,expr,recombine=FALSE,...){
    parent <- parent.frame()
    non.null <- sapply(data,length) > 0
    mc <- match.call()
    ii <- attr(data,"groups")
    av <- all.vars(mc$expr)
    av <- intersect(av,names(data))
    res <- lapply(ii,within1.grouped.data.frame,data[av],mc$expr,parent=parent.frame())
    n <- length(res)
    m <- length(res[[1]])
    nms <- names(res[[1]])
    res <- unlist(res,recursive=FALSE)
    dim(res) <- c(m,n)
    res <- t(res)
    res <- lapply(1:m,FUN=rbind1col,mat=res)
    names(res) <- nms
    ii <- unlist(ii)
    res <- lapply(res,FUN=reorder1,ii=ii)
    res <- rev(res)
    data[names(res)] <- res
    return(data)
}

with1 <- function(ii,data,expr,parent=parent.frame(),...){
    if(!length(ii)) res <- NULL
    else
        res <- eval(expr, data[ii,,drop=FALSE], enclos = parent)
    res
}

with.grouped.data <- function(data,expr,...){
    non.null <- sapply(data,length) > 0
    mc <- match.call()
    ii <- attr(data,"groups")
    av <- all.vars(mc$expr)
    av <- intersect(av,names(data))
    #with1. <- function(ii) eval(mc$expr,data[ii,av,drop=FALSE],enclos=parent.frame())
    #browser()
    res_ <- lapply(ii,with1,data[av],mc$expr,parent=parent.frame())
    #system.time(res_ <- lapply(ii,with1.))#,data[av],mc$expr)
    spec_ <- attr(data,"spec")
    
    if(all(non.null)){
        res_len <- sapply(res_,length)
        if(length(unique(res_len))==1) {
            # All results have the same length
            if(length(dim(res_[[1]]))){
                newdim <- c(dim(res_[[1]]),dim(ii))
                newdimnames <- c(dimnames(res_[[1]]),dimnames(ii))
            } else if(length(res_[[1]]) > 1){
                newdim <- c(length(res_[[1]]),dim(ii))
                newdimnames <- c(list(names(res_[[1]])),dimnames(ii))
            } else {
                newdim <- c(dim(ii))
                newdimnames <- c(dimnames(ii))
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

# names.grouped.data <- function(x){
#     names(x[[1]])
# }


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
    call_[[1]] <- within
    call_$data <- data
    call_$recombine <- recombine
    eval(call_,envir=parent.frame())
}

