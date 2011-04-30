to.data.frame <- function(X,as.vars=1,name="Freq"){
  if(is.atomic(X)){
    as.vars <- as.vars[1]
    if(as.vars==0){
        Z <- dimnames(X)
        if(!length(Z)){
          Z <- lapply(dim(X),seq_len)
        }
        else {
          Znull <- sapply(Z,length) == 0
          Zdims <- dim(X)
          Z[Znull] <- lapply(Zdims[Znull],seq_len)
        }
        Z <- numericIfPossible(expand.grid(Z))
        X <- as.data.frame(structure(list(c(X)),names=name))
    }
    else {
        ncols <- dim(X)[as.vars]
        nrows <- prod(dim(X)[-as.vars])
        coln <- dimnames(X)[[as.vars]]
        Z <- dimnames(X)[-as.vars]
        if(!length(Z)){
          Z <- lapply(dim(X)[-as.vars],seq_len)
        }
        else {
          Znull <- sapply(Z,length) == 0
          Zdims <- dim(X)[-as.vars]
          Z[Znull] <- lapply(Zdims[Znull],seq_len)
        }
        Z <- numericIfPossible(expand.grid(Z))
        ii <- seq(length(dim(X)))
        X <- aperm(X,c(ii[-as.vars],ii[as.vars]))
        dim(X) <- c(nrows,ncols)
        X <- as.data.frame.matrix(X)
        rownames(X) <- rownames(Z) <- 1:nrows
        names(X) <- coln
        }
    }
  else {
    nrows <- prod(dim(X))
    Z <- dimnames(X)
    if(!length(Z)){
      Z <- lapply(dim(X),seq_len)
    }
    else {
      Znull <- sapply(Z,length) == 0
      Zdims <- dim(X)
      Z[Znull] <- lapply(Zdims[Znull],seq_len)
    }
    X <- lapply(X,as.data.frame)
    Z <- numericIfPossible(expand.grid(Z))
    lnrows <- sapply(X,nrow)
    lncols <- sapply(X,ncol)
    if(!allequal(lncols)) stop("array elements do not match")
    ncols <- lncols[1]
    X <- do.call("rbind",X)
    i <- rep(1:nrows,lnrows)
    Z <- Z[i,,drop=FALSE]
    rownames(X) <- rownames(Z) <- 1:nrow(X)
    }
  cbind(Z,X)
}

# to.array <- function(x,...) UseMethod("to.array")
# to.array.default <- function(x,...)base::as.array(x)

# to.array.data.frame <- function(x,data.name=NULL,...){
#   s <- paste(deparse(substitute(x)),collapse="")
#   fcts <- sapply(x,is.factor)
#   if(any(fcts)){
#     nofcts <- !fcts
#     fcts <- x[fcts]
#     d <- c(sum(nofcts),sapply(fcts,nlevels))
#     m <- nrow(x) %/% prod(d[-1])
#     r <- nrow(x) %% prod(d[-1])
#     if(r!=0)stop(gettextf("cannot transform '%s' into an array",s))
#     rn <- rownames(x)
#     x <- x[nofcts]
#     ii <- c(list(rn),lapply(fcts,function(x)match(x,levels(x))))
#     x <- as.matrix(x)
#     x <- t(x)[,do.call("order",rev(ii))]
#     dn <- c(structure(list(rownames(x)),names=data.name),lapply(fcts,levels))
#     if(m>1){
#       dim(x) <- c(m,d)
#       dimnames(x) <- c(list(rn[1:m]),dn)
#     }
#     else {
#       dim(x) <- d
#       dimnames(x) <- dn
#     }
#     return(x)
#   }
#   else return(as.matrix(x))
# }

setMethod("as.array","data.frame",
  function(x,data.name=NULL,...){
    s <- paste(deparse(substitute(x)),collapse="")
    fcts <- sapply(x,is.factor)
    if(any(fcts)){
      nofcts <- !fcts
      fcts <- x[fcts]
      d <- c(sum(nofcts),sapply(fcts,nlevels))
      m <- nrow(x) %/% prod(d[-1])
      r <- nrow(x) %% prod(d[-1])
      if(r!=0)stop(gettextf("cannot transform '%s' into an array",s),call.=FALSE)
      rn <- rownames(x)
      x <- x[nofcts]
      ii <- c(list(rn),lapply(fcts,function(x)match(x,levels(x))))
      x <- as.matrix(x)
      x <- t(x)[,do.call("order",rev(ii))]
      dn <- c(structure(list(rownames(x)),names=data.name),lapply(fcts,levels))
      if(m>1){
        dim(x) <- c(m,d)
        dimnames(x) <- c(list(rn[1:m]),dn)
      }
      else {
        dim(x) <- d
        dimnames(x) <- dn
      }
      return(x)
    }
    else return(as.matrix(x))
})
