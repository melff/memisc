"%nin%" <- function(x, table) !(x %in% table)

reorder.array <- function(x,dim=1,names=NULL,indices=NULL,FUN=mean,...){
  if(length(dim)>1) stop("'dim' has to be scalar")
  if(as.numeric(!missing(names)) + as.numeric(!missing(indices)) + as.numeric(!missing(FUN))>2) 
    stop("too many arguments, use *either* 'names', 'indices', or 'FUN'")
  if(!missing(names)){
    indices <- match(names,dimnames(x)[[dim]])
    if(any(is.na(indices))) stop("unused dinnames in 'names' argument")
    other.indices <- setdiff(1:dim(x)[dim],indices)
    sort.indices <- c(indices,other.indices)
  }
  else if(!missing(indices)){
    other.indices <- setdiff(1:dim(x)[dim],indices)
    sort.indices <- c(indices,other.indices)
  } else {
    results <- apply(x,dim,FUN)
    sort.indices <- order(results)
  } 
  all.indices <- lapply(dim(x),function(d)1:d)
  all.indices[[dim]] <- sort.indices
  do.call("[",c(list(x),all.indices))
}

reorder.matrix <- reorder.array

lsl <- function() ls.str(envir=parent.frame())

split.matrix <- function (x, f, drop = FALSE, dim.drop=TRUE, ...){
  index <- seq(nrow(x))
  if(length(index)==length(f)){
    index <- split.default(index,f,drop=drop)
    lapply(index,function(i)x[i,,drop=dim.drop])
  } else {
    split(c(x),f,drop=drop,...)
  }
}

"split<-.matrix" <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq_along(x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j%%n + 1
        x[i, ] <- value[[j]]
    }
    x
}
