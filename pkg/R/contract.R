muq <- function(x,sort=FALSE,drop.na=TRUE){
    u <- unique(x)
    if(drop.na)
        u <- na.omit(u)
    if(sort)
        u <- sort(u)
    structure(match(x,u),
              unique=u)
}

contract <- function(x,...) UseMethod("contract")

contract.data.frame <- function(x,
                                by=NULL,
                                weights=NULL,
                                name="Freq",
                                force.name=FALSE,
                                sort=FALSE,
                                drop.na=TRUE,
                                ...){
    w <- NULL
    if(!missing(by)){
        tmp <- as.list(seq_along(x))
        names(tmp) <- names(x)
        by <- eval(substitute(by),tmp,parent.frame())
        if(inherits(by,"formula")){
            if(length(by)>2){
                w <- eval(by[[2]],x,parent.frame())
                by <- by[-2]
            }
            by <- all.vars(by)
        }
    }
    if(!missing(weights)) {
        weights <- eval(substitute(weights),x,parent.frame())
        if(!is.numeric(weights)) stop("'weights' must be numeric")
        if(length(w) && is.numeric(w)) warning("Ignoring extraneous 'weights' argument")
        if(!length(w))
            w <- weights
    }
    if(!missing(by))
        x <- x[by]
        
    i <- lapply(x,muq,sort=sort,drop.na=drop.na)
    u <- lapply(i,attr,"unique")
    l <- sapply(u,length)
    n <- length(i)
    j <- i[[1]]
    if(n > 1){
        r <- l[1]
        for(k in 2:n){
            i.k <- i[[k]]
            j <- j + r*(i.k-1)
            j <- muq(j,sort=sort,drop.na=drop.na)
            r <- length(attr(j,"unique"))
        }
    }
    if(!length(w)){
        f <- tabulate(j)
    }
    else {
        if(length(w) != nrow(x))
            stop("'weights' argument has wrong length")
        if(any(is.na(w)))
            warning("NA weights treated as zero")
        if(is.factor(w)){
            w <- dummyMatrix(w)
            if(!missing(weights))
                w <- w*weights
        }
        if(drop.na && any(is.na(j))){
            keep <- !is.na(j)
            f <- rowsum.default(w[keep],j[keep],na.rm=TRUE)
        }
        else
            f <- rowsum.default(w,j,na.rm=TRUE)
    }
    if(drop.na)
        y <- x[!duplicated(j) & !is.na(j),,drop=FALSE]
    else
        y <- x[!duplicated(j),,drop=FALSE]
    if(sort){
        uj <- unique(j)
        y <- y[order(uj),,drop=FALSE]
    }
    rownames(y) <- 1:nrow(y)
    if(NCOL(f) > 1){
        if(force.name)
            colnames(f) <- paste(name,colnames(f),sep=".")
        nms <- c(colnames(y),colnames(f))
        y <- cbind(y,f)
        names(y) <- nms
    }
    else
        y[[name]] <- drop(f)
    y
}

contract.data.set <- contract.data.frame

dummyMatrix <- function(f){
    stopifnot(is.factor(f))
    l <- levels(f)
    res <- matrix(nrow=length(f),ncol=length(l))
    for(i in seq_along(l)){
        res[,i] <- as.integer(f==l[i])
    }
    colnames(res) <- l
    res
}
