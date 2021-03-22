

centerAt <- function(x,at=getOption("OutDec"),integers=c("dot","right","left"),skip=0){
  has.dot <- setdiff(grep(at,x,fixed=TRUE),skip)
  if(!any(has.dot>0)) return(x)
  x <- trimws(x)
  is.int <- setdiff(seq(x),union(has.dot,skip))
  splitted <- strsplit(x[has.dot],at,fixed=TRUE)
  left <- sapply(splitted,function(x)x[1])
  maxleft <- max(nchar(left))
  right <- sapply(splitted,function(x)paste(x[-1],collapse="."))
  maxright <- max(nchar(right))
  maxcentered <- maxleft+maxright+1
  
  if(any(is.int>0)){
    integers <- match.arg(integers)
    if(integers=="right"){
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="right",
                          width=maxcentered)
    }
    if(integers=="left"){
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="left",
                          width=maxcentered)
    }
    if(integers=="dot"){
      maxleft <- max(maxleft,max(nchar(as.character(x[is.int]))))
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="right",
                          width=maxleft)
      fintegers <- paste(fintegers,format(" ",width=maxright))
    }
    centered <- paste(left,right,sep=".")
    maxcentered <- max(nchar(centered))
    x[has.dot] <- centered
    x[is.int] <- fintegers
  } else {
    left <- format(left,justify="right",width=maxleft)
    right <- format(right,justify="left",width=maxright)
    centered <- paste(left,right,sep=".")
    x[has.dot] <- centered
  }
  if(any(as.logical(skip)))
    x[skip] <- format(x[skip],width=maxcentered,justify="centre")
  
  return(x)
}

coefxpand <- function(x,names){
    if(length(x)){
        d <- dx <- dim(x)
        dd <- ddx <- dimnames(x)
        ddNULL <- sapply(dd,is.null)
        d[3] <- length(names)
        dd[[3]] <- names
        dd_ <- dd
        dd[ddNULL] <- lapply(d[ddNULL],seq,from=1)
        ddx[ddNULL] <- dd[ddNULL]
        res <- array("",dim=d,dimnames=dd)
        call.arg <- list(res)
        call.arg <- c(call.arg,ddx)
        call.arg <- c(call.arg,list(value=as.vector(x)))
        res <- do.call("[<-",call.arg)
        dimnames(res) <- dd_
        res
    }
    else if(length(dim(x))){
        d <- dim(x)
        dd <- dimnames(x)
        d[3] <- length(names)
        dd[[3]] <- names
        array("",dim=d,dimnames=dd)
    } else {
        r <- length(names)
        array("",dim=c(r,1,1),
              dimnames=list(names,NULL,NULL))
    }
}

smryxpand <- function(x,names){
  
    res <- matrix(rep("",length(names)),ncol=1)
    rownames(res) <- names              
    if(length(x)) {
        nms.x <- rownames(x)
        res[nms.x,] <- x
    }
    return(res)
}
