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
    expr <- mc$expr
    ii <- attr(data,"groups")
    av <- all.vars(mc$expr)
    av <- intersect(av,names(data))
    #with1. <- function(ii) eval(mc$expr,data[ii,av,drop=FALSE],enclos=parent.frame())
    #browser()
    res <- lapply(ii,with1,data[av],expr,parent=parent.frame())
    #system.time(res_ <- lapply(ii,with1.))#,data[av],mc$expr)
    spec_ <- attr(data,"spec")

    non.null <- !sapply(res,is.null)
    res_ <- res[non.null]
    
    res_len <- sapply(res_,length)

    if(length(unique(res_len))==1) {
            # All results have the same length
        if(length(dim(res_[[1]]))){
            newdim <- c(dim(res_[[1]]),dim(ii))
            if(length(dimnames(res_[[1]])))
                newdimnames <- c(dimnames(res_[[1]]),dimnames(ii))
            else
                newdimnames <- c(rep(list(NULL),length(dim(res_[[1]]))),
                                 dimnames(ii))
        } else if(length(res_[[1]]) > 1){
            newdim <- c(length(res_[[1]]),dim(ii))
            newdimnames <- c(list(names(res_[[1]])),dimnames(ii))
        } else {
            newdim <- dim(ii)
            newdimnames <- dimnames(ii)
        }
        if(all(sapply(res_,is.atomic))) {
            if(all(non.null)){
                res <- unlist(res_)
            }
            else {
                res_len1 <- res_len[1]
                res <- rep(NA,length(res)*res_len1)
                res[rep(non.null,each=res_len1)] <- unlist(res_)
            }
            
        } else {
            res[non.null] <- unlist(res_,recursive=FALSE)
        }
        dim(res) <- newdim
        dimnames(res) <- newdimnames
    }
    else {
        dim(res) <- dim(ii)
        dimnames(res) <- dimnames(ii)
    }

    if(length(dim(res)) >= 2){
        if(is.null(rownames(res))){
            if(nrow(res)==1) rownames(res) <- deparse(expr)
            else if( as.character(expr[[1]]) %in% c("c","cbind","rbind")
                    && length(expr[-1]) == nrow(res))
                rownames(res) <- paste(expr[-1])
            else if(as.character(expr[[1]]) %in% c("range"))
                rownames(res) <- c("Min","Max")
        }
    }
    
    class(res) <- "grouped.result"
    attr(res,"spec") <- spec_
    if(!all(non.null)){
        empty <- !non.null
        dim(empty) <- dim(ii)
        attr(res,"empty") <- empty
    }
    if(is.atomic(res))
        class(res) <- c(class(res),"table")
    res
}

print.grouped.result <- function(x,...){
    attr(x,"spec") <- NULL
    attr(x,"empty") <- NULL
    class(x) <- NULL
    print.default(x)
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
    call_[[1]] <- within
    call_$data <- data
    call_$recombine <- recombine
    eval(call_,envir=parent.frame())
}

