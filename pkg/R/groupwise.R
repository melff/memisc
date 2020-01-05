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

get_all_vars <- function(formula,data,inherits=TRUE){
    formula.vars <- all.vars(formula)
    data.vars <- names(data)
    if(all(formula.vars %in% data.vars))
        return(data[formula.vars])
    else {
        other.vars <- setdiff(formula.vars,data.vars)
        data.vars <- intersect(formula.vars,data.vars)
        res <- data[data.vars]
        other.vars <- mget(other.vars,inherits=inherits)
        res[names(other.vars)] <- other.vars
        return(res)
    }
}

Groups <- function(data,by,...) UseMethod("Groups")
    
Groups.data.frame <- function(data,by,...){
    by.vars <- get_all_vars(by,data)
    factors <- lapply(by.vars,as_factor)
    ii <- 1:nrow(data)
    ii <- tapply(ii,factors,I)
    n.ii <- sapply(ii,length)
    dim(n.ii) <- dim(ii)
    extra.attr <- list(groups=ii,
                       sizes=n.ii,
                       spec=by.vars[0,,drop=FALSE],
                       class=c(paste("grouped",class(data)[1],sep="."),"grouped.data",class(data)))
    attributes(data) <- c(attributes(data),extra.attr)
    data
}


Groups.data.set <- function(data,by,...){
    by.vars <- get_all_vars(by,data)
    factors <- lapply(by.vars,as_factor)
    data <- structure(data@.Data,
                      names=names(data),
                      row.names=data@row_names,
                      class="data.frame")
    ii <- 1:nrow(data)
    ii <- tapply(ii,factors,I)
    n.ii <- sapply(ii,length)
    dim(n.ii) <- dim(ii)
    extra.attr <- list(groups=ii,
                       sizes=n.ii,
                       spec=by.vars[0,,drop=FALSE],
                       class=c("grouped.data.set","grouped.data",class(data)))
    attributes(data) <- c(attributes(data),extra.attr)
    data
}

Groups.grouped.data <- function(data,by,...)Groups(recombine(data),by,...)

ngroups <- function(x){
    sum(attr(x,"sizes") > 0)
}  

eval1call <- function(data,call,envir){
    call$data <- data
    eval(call,envir=envir)
}

mklen <- function(x,n){
    y <- vector(length=n,mode=mode(x))
    y[] <- x
    return(y)
}

first <- function(x) x[[1]]
last <- function(x) x[[length(x)]]
set_first <- function(x,value){
    x[[1]] <- value
    x
}
set_last <- function(x,value){
    x[[length(x)]] <- value
    x
}

fill_dimnames2 <- function(dn,d){
    isn <- sapply(dn,length) == 0
    dn[isn] <- lapply(d[isn],seq.int)
    dn
}

within1.grouped.data <- function(ii,data,expr,N_,parent=parent.frame(),...){
    if(!length(ii)) return(NULL)
    encl <- new.env(parent=parent)
    n <- length(ii)
    assign("n_",n,envir=encl)
    assign("N_",N_,envir=encl)
    assign("i_",ii,envir=encl)
    e <- evalq(environment(),data[ii,,drop=FALSE],encl)
    eval(expr,e)
    l <- as.list(e,all.names=TRUE)
    # l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
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

within.grouped.data <- function(data,expr,recombine=FALSE,...){

    parent <- parent.frame()
    mc <- match.call()

    groups <- attr(data,"groups")
    sizes <- attr(data,"sizes")
    non_empty <- as.vector(sizes) > 0
    ii <- groups[non_empty]

    av <- all.vars(mc$expr)
    av <- intersect(av,names(data))

    res <- lapply(ii,within1.grouped.data,data[av],mc$expr,N_=nrow(data),parent=parent.frame())

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
    
    if(recombine)
        return(recombine(data))
    else 
        return(data)
}

with1 <- function(ii,data,expr,N_,parent=parent.frame(),...){
    encl <- new.env(parent=parent)
    assign("n_",length(ii),envir=encl)
    assign("N_",N_,envir=encl)
    assign("i_",ii,envir=encl)
    eval(expr, data[ii,,drop=FALSE], enclos = encl)
}

add_tags <- function(expr,taggables=c("c","list","cbind","rbind")){
    is_taggable <- as.character(expr[[1]]) %in% taggables
    if(is_taggable){
        expr_1 <- expr[-1]
        n <- length(expr_1)
        if(!length(names(expr_1))){
            untagged <- rep(TRUE,n)
            names(expr) <- c("",letters[seq.int(n)])
        }
        else
            untagged <- !nzchar(names(expr_1))
        if(any(untagged)){
            names(expr)[-1][untagged] <- sapply(expr_1[untagged],deparse)
        }
    }
    expr
}

with.grouped.data <- function(data,expr,...){
    mc <- match.call()
    expr <- mc$expr
    groups <- attr(data,"groups")
    sizes <- attr(data,"sizes")
    non_empty <- as.vector(sizes) > 0
    av <- all.vars(mc$expr)
    av <- intersect(av,names(data))
    #with1. <- function(ii) eval(mc$expr,data[ii,av,drop=FALSE],enclos=parent.frame())
    expr <- add_tags(expr)
    
    ii <- groups[non_empty]
    res_ <- lapply(ii,with1,data[av],expr,N_=nrow(data),parent=parent.frame())
    #system.time(res_ <- lapply(ii,with1.))#,data[av],mc$expr)
    spec_ <- attr(data,"spec")

    res_len <- sapply(res_,length)

    if(length(unique(res_len))==1) {
        # All results have the same length
        res_len <- res_len[1]
        if(length(dim(res_[[1]]))){
            dim_res <- dim(res_[[1]])
            newdim <- c(dim_res,dim(groups))
            if(length(res_dimnames <- dimnames(res_[[1]]))){
                newdimnames <- c(res_dimnames,dimnames(groups))
            }
            else
                newdimnames <- c(rep(list(NULL),length(dim_res)),
                                 dimnames(groups))
        } else if(length(res_[[1]]) > 1){
            newdim <- c(length(res_[[1]]),dim(groups))
            newdimnames <- c(list(names(res_[[1]])),dimnames(groups))
        } else {
            newdim <- dim(groups)
            newdimnames <- dimnames(groups)
        }
        if(all(sapply(res_,is.atomic))) {
            if(all(non_empty)){
                res <- unlist(res_)
            }
            else {
                res <- rep(NA,length(groups)*res_len)
                res[rep(non_empty,each=res_len)] <- unlist(res_)
            }
            
        } else {
            res <- vector(mode="list",length=length(groups)*res_len)
            res[rep(non_empty,each=res_len)] <- unlist(res_,recursive=FALSE)
        }
        dim(res) <- newdim
        dimnames(res) <- fill_dimnames2(newdimnames,newdim)
    }
    else {
        res <- vector(mode="list",length=length(groups))
        res[non_empty] <- unlist(res_,recursive=FALSE)
        dim(res) <- dim(groups)
        dimnames(res) <- dimnames(groups)
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
    attr(res,"sizes") <- sizes
    if(is.atomic(res))
        class(res) <- c(class(res),"table")
    res
}

print.grouped.result <- function(x,...){
    attr(x,"spec") <- NULL
    attr(x,"sizes") <- NULL
    class(x) <- NULL #setdiff(class(x),"grouped.result")
    print.default(x)
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

recombine <- function(x,...) UseMethod("recombine")

recombine.grouped.data.frame <- function(x,...) {
    attributes(x)[c("spec","sizes","groups")] <- NULL
    class(x) <- "data.frame"
    return(x)
}

recombine.grouped.data.set <- function(x,...) {
    attributes(x)[c("spec","sizes","groups")] <- NULL
    class(x) <- NULL
    return(new("data.set",x))
}


print.grouped.data.frame <- function(x,...){
  ngrps <- ngroups(x)  
  cat("\nGrouped data frame with",nrow(x), "observations in",ngrps,"groups of",ncol(x),"variables\n\n")
  print_frame_internal(x,max.obs=getOption("show.max.obs"),width=getOption("width"),...)
}

print.grouped.data.set <- function(x,...){
  ngrps <- ngroups(x)  
  cat("\nGrouped data frame with",nrow(x), "observations in",ngrps,"groups of",ncol(x),"variables\n\n")
  print_frame_internal(x,max.obs=getOption("show.max.obs"),width=getOption("width"),...)
}


as.data.set.grouped.data <- function(x,...){
    as.data.set(recombine(x),...)
}

as.data.frame.grouped.data <- function(x,...){
    as.data.frame(recombine(x),...)
}

setOldClass("grouped.data.set")
setOldClass("grouped.data.frame")
setMethod("as.data.set","grouped.data.set",as.data.set.grouped.data)
setMethod("as.data.set","grouped.data.frame",as.data.set.grouped.data)
