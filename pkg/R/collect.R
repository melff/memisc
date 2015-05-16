collect <- function(...,names=NULL,inclusive=TRUE) {
  if(!length(match.call(expand.dots=FALSE)$...)) return(NULL)
  else UseMethod("collect",..1)
}

collect.default <- function(...,names=NULL,inclusive=TRUE){
  if(!is.atomic(..1)) stop("cannot handle arguments of mode ",mode(..1))
  args <- list(...)
  subst <- substitute(list(...))
  if(length(names)) {
    if(length(names)!=length(args)) stop("names argument has wrong length")
  }
  else {
    if(length(names(args))) names <- names(args)
    else {
      names <- sapply(lapply(subst[-1],deparse),paste,collapse=" ")
    }
  }
  modes <- sapply(args,mode)
  if(!inclusive){
    common.names <- reduce(lapply(args,names),intersect)
    args <- lapply(args,function(x)x[common.names])
  }
  if(any(modes!=mode(..1))){
    for(i in seq_along(args))
      mode(args[[i]]) <- mode(..1)
  }
  res <- clct.vectors(args)
  colnames(res) <- names
  res
}

collect.matrix <- collect.array <- function(...,names=NULL,inclusive=TRUE){
  if(!is.atomic(..1)) stop("cannot handle arguments of mode ",mode(..1))
  args <- list(...)
  subst <- substitute(list(...))
  if(length(names)) {
    if(length(names)!=length(args)) stop("names argument has wrong length")
  }
  else {
    if(length(names(args))) names <- names(args)
    else {
      names <- sapply(lapply(subst[-1],deparse),paste,collapse=" ")
    }
  }
  modes <- sapply(args,mode)
  if(any(modes!=mode(..1))){
    for(i in seq_along(args))
      mode(args[[i]]) <- mode(..1)
  }
  dims <- unique(sapply(args,function(x)length(dim(x))))
  if(length(dims)>1) stop("array dims do not match")
  
  if(!inclusive){
    name.select.args <- vector(mode="list",length=dims)
    for(i in 1:dims){
      common.names <- reduce(lapply(args,function(x)dimnames(x)[[i]]),intersect)
      name.select.args[[i]] <- common.names
    }
    for(i in seq_along(args))
      args[[i]] <- do.call("[",c(args[i],name.select.args))
  }
  
  res <- clct.arrays(args)
  dimnames(res)[[length(dim(res))]] <- names
  res
}

collect.table <- function(...,names=NULL,sourcename=".origin",fill=0){
  if(!is.atomic(..1)) stop("cannot handle arguments of mode ",mode(..1))
  args <- list(...)
  subst <- substitute(list(...))
  if(length(names)) {
    if(length(names)!=length(args)) stop("names argument has wrong length")
  }
  else {
    if(length(names(args))) names <- names(args)
    else {
      names <- sapply(lapply(subst[-1],deparse),paste,collapse=" ")
    }
  }
  one.dim <- all(sapply(args,function(x)length(dim(x)))==1)
  if(one.dim){
    args <- lapply(args,c)
    res <- clct.vectors(args)
    common.names <- "Freq"
  }
  else {
    common.names <- reduce(lapply(args,function(x)names(dimnames(x))),intersect)
    if(!length(common.names)) return(NULL)
    args <- lapply(args,function(x){
      ndn <- names(dimnames(x))
      mdims <- which(ndn %in% common.names)
      margin.table(x,mdims)
    })
    res <- clct.arrays(args)
  }
  res[is.na(res)] <- fill
  dimnames(res)[[length(dim(res))]] <- names
  names(dimnames(res)) <- c(common.names,sourcename)
  as.table(res)
}

collect.data.frame <- function(...,
  names=NULL,inclusive=TRUE,fussy=FALSE,warn=TRUE,
  sourcename=".origin"){
  args <- list(...)
  subst <- substitute(list(...))
  if(length(names)) {
    if(length(names)!=length(args)) stop("names argument has wrong length")
  }
  else {
    if(length(names(args))) names <- names(args)
    else {
      names <- sapply(lapply(subst[-1],deparse),paste,collapse=" ")
    }
  }
  all.vars <- lapply(args,names)
  common.vars <- reduce(all.vars,intersect)
  all.vars <- reduce(all.vars,union)
  other.vars <- setdiff(all.vars,common.vars)
  source <- rep(seq_along(args),sapply(args,nrow))
  nrow.items <- sapply(args,nrow)
  nrow.total <- sum(nrow.items)
  ix <- split(seq_len(nrow.total),source)
  res <- lapply(common.vars,function(var){
                vecs <- lapply(args,function(x)x[[var]])
                collOne(vecs,source=source,nrow.items=nrow.items,varname=var,fussy=fussy)
                })
  names(res) <- common.vars
  if(inclusive){
    res1 <- lapply(other.vars,function(var){
                  vecs <- lapply(args,function(x)x[[var]])
                  collOne(vecs,source=source,nrow.items=nrow.items,varname=var,fussy=fussy)
                  })
    names(res1) <- other.vars
    res <- c(res,res1)
  }
  res[[sourcename]] <- factor(source,labels=names)
  as.data.frame(res)
}

collect.data.set <- function(...,
  names=NULL,inclusive=TRUE,fussy=FALSE,warn=TRUE,
  sourcename=".origin"){
  args <- list(...)
  subst <- substitute(list(...))
  if(length(names)) {
    if(length(names)!=length(args)) stop("names argument has wrong length")
  }
  else {
    if(length(names(args))) names <- names(args)
    else {
      names <- sapply(lapply(subst[-1],deparse),paste,collapse=" ")
    }
  }
  all.vars <- lapply(args,names)
  common.vars <- reduce(all.vars,intersect)
  all.vars <- reduce(all.vars,union)
  other.vars <- setdiff(all.vars,common.vars)
  source <- rep(seq_along(args),sapply(args,nrow))
  nrow.items <- sapply(args,nrow)
  nrow.total <- sum(nrow.items)
  ix <- split(seq_len(nrow.total),source)
  res <- lapply(common.vars,function(var){
                vecs <- lapply(args,function(x)x[[var]])
                collOne(vecs,source=source,nrow.items=nrow.items,varname=var,fussy=fussy)
                })
  names(res) <- common.vars
  if(inclusive){
    res1 <- lapply(other.vars,function(var){
                  vecs <- lapply(args,function(x)x[[var]])
                  collOne(vecs,source=source,nrow.items=nrow.items,varname=var,fussy=fussy)
                  })
    names(res1) <- other.vars
    res <- c(res,res1)
  }
  res[[sourcename]] <- factor(source,labels=names)
  as.data.set(res)
}


toArray <- function(x){
  if(!length(dim(x))) as.matrix(x)
  else x
}

collect.by <- function(...,inclusive=TRUE,names=NULL){
  dn <- dimnames(..1)
  dd <- dim(..1)
  res <- if(any(sapply(..1,is.array)))
          clct.arrays(lapply(..1,toArray))
         else
          clct.vectors(..1)
  dd <- c(dim(res)[-length(dim(res))],dd)
  dn <- c(dimnames(res)[-length(dim(res))],dn)
  dim(res) <- dd
  dimnames(res) <- dn
  res
}


reduce <- function(x,FUN,...){
  lx <- length(x)
  if(lx<2) return(FUN(x[[1]],...))
  FUN <- match.fun(FUN)
  y <- x[[1]]
  for(i in 2:lx)
    y <- FUN(y,x[[i]],...)
  return(y)
}

collOne <- function(vecs,source,nrow.items,varname,fussy=FALSE,warn=TRUE){
  lens <- sapply(vecs,length)
  has.els <- lens > 0
  checkAttribs(vecs[which(has.els)],varname=varname,fussy=fussy)
#   browser()
  first.nonempty <- which(has.els)[1]
  res <- lapply(seq_along(nrow.items),
                  function(i)
                    if(has.els[i]) vecs[[i]]
                    else rep(NA,nrow.items[i])
                  )
  res <- unsplit(res,source)
  attributes(res) <- attributes(vecs[[first.nonempty]])
  res
}


checkAttribs <- function(x,varname,fussy=FALSE,warn=TRUE){
  modes <- sapply(x,mode)
  if(length(unique(modes))>1) stop("modes for ",sQuote(varname)," do not match",call.=FALSE)
  attribs <- lapply(x,attributes)
  attribs.identical <- namedlists.pairwise.identical(attribs)
  if(!attribs.identical && fussy) stop("attributes for ",sQuote(varname)," do not match",call.=FALSE)
  if(!attribs.identical && warn) warning("attributes for ",sQuote(varname)," do not match",call.=FALSE)
}

namedlists.pairwise.identical <- function(x){
  if(length(x) == 1) return(TRUE)
  x[[1]] <- x[[1]][suppressWarnings(sort(names(x[[1]])))]
  res <- TRUE
  if(length(x)>1){
    for(i in 2:length(x))
      res <- res && identical(x[[1]],x[[i]][suppressWarnings(sort(names(x[[i]])))])
    }
  res
}

ord.union <- function(x,y) .Call("ord_union",x,y)

fill_dimnames <- function(x){
  d <- length(dim(x))
  dd <- dim(x)
  dn <- dimnames(x)
  if(length(dn)){
    for(i in 1:d)
      if(!length(dn[[i]])) dn[[i]] <- seq_len(dd[i])
  }
  else {
    dn <- list()
    for(i in 1:d)
      dn[[i]] <- seq_len(dd[i])
  }
  dimnames(x) <- dn
  x
}

clct.arrays <- function(x){
  dims <- Sapply(x,dim)
  n.x <- length(x)
  if(is.list(dims)) stop("Dimensions do not match")
  x <- lapply(x,fill_dimnames)
  cdimnames <- vector(mode="list",length=nrow(dims))
  for(i in 1:nrow(dims)){
    for(j in 1:n.x){
      if(j==1) cdimnames[[i]] <- dimnames(x[[1]])[[i]]
      else cdimnames[[i]] <- union(cdimnames[[i]], dimnames(x[[j]])[[i]])
    }
  }
  cdims <- sapply(cdimnames,length)
  cdims <- c(cdims,n.x)
  res <- array(NA,dim=cdims,dimnames=c(cdimnames,list(NULL)))
#   str(res)
  for(j in 1:n.x){
    rhs <- list(as.symbol("["),as.call(list(as.symbol("[["),as.symbol("x"),j)))
    rhs <- as.call(rhs)
    lhs <- c(as.symbol("[<-"),as.symbol("res"),dimnames(x[[j]]),list(j))
    ex <- as.call(c(lhs,rhs))
    res <- eval(ex)
  }
  res
}

clct.vectors <- function(x){
  n.x <- length(x)
  for(j in 1:n.x){
    if(j==1) cnames <- names(x[[1]])
    else cnames <- union(cnames,names(x[[j]]))
    }
  if(length(cnames)){
    res <- matrix(NA,nrow=length(cnames),ncol=n.x)
    rownames(res) <- cnames
    for(j in 1:n.x){
      res[names(x[[j]]),j] <- x[[j]]
      }
    }
  else{
    lenx <- sapply(x,length)
    if(length(unique(lenx))>1){
      maxlen <- max(lenx)
      x <- lapply(x,"length<-",maxlen)
    }
    res <- do.call(cbind,x)
  }
  if(length(names(x))) colnames(res) <- names(x)
  res
}


clct.lists <- function(x){
  ncol <- length(x)
  lnames <- lapply(x,names)
  unames <- unique(unlist(lnames))
  nrow <- length(unames)
  res <- matrix(list(),nrow,ncol)
  dimnames(res) <- list(unames,names(x))
  for(i in seq_along(x))
    res[names(x[[i]]),i] <- x[[i]]
  res
}
