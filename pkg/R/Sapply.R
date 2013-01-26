
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




