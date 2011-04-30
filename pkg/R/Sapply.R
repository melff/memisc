SapplyOLD <- function (X, FUN, ..., test.dim=FALSE, simplify = TRUE, USE.NAMES = TRUE){
  sapply.call <- match.call()
  sapply.call$test.dim <- NULL
  sapply.call[[1]] <- as.name("sapply")
  Y <- eval(sapply.call,parent.frame())
  if(!test.dim && !is.array(X)) return(Y)
  dimX <- dim(X)
  dimnamesX <- dimnames(X)
  dimY <- dim(Y)
  if(!length(dimY)){
    return(structure(unname(Y),dim=dimX,dimnames=dimnamesX))
  }
  else {
    if(test.dim && length(dim(Y1 <- FUN(X[[1]])))) {
      dimY <- c(dim(Y1),dimX)
      if(length(dimnames(Y1)))
        dimnamesY <- c(dimnames(Y1),dimnamesX)
      else
        dimnamesY <- c(vector(mode="list",length=length(dim(Y1))),dimnamesX)
    }
    else {
      dimY <- c(dim(Y)[1],dimX)
      if(length(rownames(Y)))
        dimnamesY <- c(dimnames(Y)[1],dimnamesX)
      else
        dimnamesY <- c(list(NULL),dimnamesX)
    }
    return(structure(unname(Y),dim=dimY,dimnames=dimnamesY))
  }
}

Sapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE){
    FUN <- match.fun(FUN)
    if(length(dim(X))){
        d.ans <- dim(X)
        dn.ans <- if(length(dimnames(X))) dimnames(X) else list(NULL) 
    } else {
        d.ans <- length(X)
        dn.ans <- if(USE.NAMES) list(names(X)) else list(NULL) 
    }
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    answer <- lapply(X,FUN,...)
    if (USE.NAMES && is.character(X) && length(d.ans) == 1 && is.null(names(answer))) 
            dn.ans <- X
    if(simplify){
        dd.ans <- NULL
        ddn.ans <- list(NULL)
        DIMS <- lapply(answer,dim)
        ulDIMS <- unique(unlist(lapply(DIMS,length)))
        if(length(ulDIMS)==1 && ulDIMS > 0){
            DIMS <- array(unlist(DIMS),dim=c(ulDIMS,length(X)))
            common.dims <- rep(NA,ulDIMS)
            for(i in seq(nrow(DIMS))){
                uDIMS.i <- unique(DIMS[i,])
                if(length(uDIMS.i) == 1){
                    common.dims[i] <- uDIMS.i
                }
            }
            if(!any(is.na(common.dims))){
            dd.ans <- common.dims
            ddn.ans <- dimnames(answer[[1]])
            }
        }
        else {
            LEN <- unique(unlist(lapply(answer,length)))
            if(length(LEN)==1 && LEN > 1){
                dd.ans <- LEN
                ddn.ans <- list(names(answer[[1]]))
                }
        }
        if(!is.null(dd.ans)){
            if(is.null(ddn.ans)) ddn.ans <- rep(list(NULL),length(dd.ans))
            return(array(unlist(answer,recursive=FALSE),dim=c(dd.ans,d.ans),dimnames=c(ddn.ans,dn.ans)))
        }
        else
            return(array(unlist(answer,recursive=FALSE),dim=c(d.ans),dimnames=c(dn.ans)))
    }
    return(array(answer,dim=d.ans,dimnames=dn.ans))
}


Lapply <- function(X, FUN, ...)
     Sapply(X, FUN, ..., simplify = FALSE, USE.NAMES = FALSE)

# Lapply <- function(X,FUN,...){
#     FUN <- match.fun(FUN)
#     if(length(dim(X))){
#         d.ans <- dim(X)
#         dn.ans <- if(length(dimnames(X))) dimnames(X) else NULL
#         if (!is.vector(X) || is.object(X))
#         X <- as.list(X)
#         return(array(
#             .Internal(lapply(X,FUN)),
#             dim=d.ans,dimnames=dn.ans))
#     }
#     else {
#         if (!is.vector(X) || is.object(X))
#         X <- as.list(X)
#         return(.Internal(lapply(X,FUN)))
#     }
# }

numericIfPossible <- function(x){
    if(is.atomic(x)) return(.numericIfPossible(x))
    else {
        res <- lapply(x,.numericIfPossible)
        attributes(res) <- attributes(x)
        return(res)
    }
}

.numericIfPossible <- function(x){
    if(is.numeric(x)) return(x) 
    else if(is.character(x)) return(.Call("numeric_if_possible", as.character(x)))
    else if(is.factor(x)) {
        levels <- .Call("numeric_if_possible",levels(x))
        if(is.numeric(levels)){
            return(levels[as.numeric(x)])
        } else return(x)
    }
    else return(x)
}

