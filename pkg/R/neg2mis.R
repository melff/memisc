neg2mis <- function(x,all=FALSE,
                    exclude=NULL,
                    select=NULL,
                    zero=FALSE){
    if(!inherits(x,"item.list"))
        stop("Argument 'x' must inherit from 'item.list'.")
    nms <- names(x)
    Dat <- structure(x@.Data,names=nms)
    if(all){
        use <- nms
    }
    else if(!missing(exclude)){
        nl <- as.list(seq_along(nms))
        names(nl) <- nms
        dont.use <- eval(substitute(exclude), nl, parent.frame())
        use <- unlist(nl[-dont.use])
    }
    else if(!missing(select)){
        nl <- as.list(seq_along(nms))
        names(nl) <- nms
        use <- eval(substitute(select), nl, parent.frame())
        use <- unlist(nl[use])
    }
    else {
        stop("one of the arguments 'all', 'exclude', or 'select' shoud be supplied")
    }
    Dat[use] <- lapply(Dat[use],neg2mis_1,zero=zero)
    x@.Data <- unname(Dat)
    x
}

neg2mis_1 <- function(x,zero=zero){
    if(length(value.filter(x))) return(x)
    else {
        if(zero)
            missing.values(x) <- list(range=c(-Inf,0))
        else
            valid.range(x) <- c(0,Inf)
        return(x)
    }
}



