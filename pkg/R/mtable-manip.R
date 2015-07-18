dim.mtable <- function(x){
  
  coefs <- x$coefficients
  ncols <- length(coefs)
  cdims <- lapply(coefs,dim)
  nrows <- sapply(cdims,"[",3)
  nrows <- max(nrows)
  c(nrows,ncols)
}

dimnames.mtable <- function(x){
  coefs <- x$coefficients
  colnames <- names(coefs)
  rownames <- lapply(coefs,dimnames)
  rownames <- lapply(rownames,"[[",3)
  rownames <- unique(unlist(rownames))
  list(
    rownames,
    colnames
  )
}

"[.mtable" <- function(x, i, j, drop = FALSE){
  

  nrows <- nrow(x)
  ncols <- ncol(x)
  rownms <- rownames(x)
  colnms <- colnames(x)
  
  mdrop <- missing(drop)
  Narg <- nargs() - (!mdrop)
  
  if(Narg<3){
    
    if(missing(i)){
      
      i <- 1:nrows
      j <- 1:ncols
    }
    else {
      
      j <- i
      i <- 1:nrows
    }
  }
  else {
    
    if(missing(i)) i <- 1:nrows
    if(missing(j)) j <- 1:ncols
  }
  
  #   return(list(Narg,i,j))(
  
  if(is.character(i)){
    i <- match(i,rownms)
    if(anyNA(i)) stop("undefined row names")
  } 
  if(is.character(j)) {
    j <- match(j,colnms)
    if(anyNA(j)) stop("undefined column names")
  }
  if(is.logical(i)) {
    tmp <- logical(nrows)
    tmp[] <- i
    i <- which(tmp)
  }
  
  if(is.logical(j)) {
    tmp <- logical(ncols)
    tmp[] <- j
    j <- which(tmp)
  }

  coefficients <- x$coefficients[j]
  
  coefficients <- lapply(coefficients, selCoef, i=i)
  
  summaries <- x$summaries[,j,drop=FALSE]
  calls <- x$calls[j]
  
  structure(list(
    coefficients=coefficients,
    summaries=summaries,
    calls=calls),
    class="mtable")
}

selCoef <- function(coef,i){
  if(length(dim(coef))==3) coef[,,i,drop=FALSE]
  else if(length(dim(coef))==4) coef[,,i,,drop=FALSE]
  else stop("This structure not yet supported")
}


##

combine_mtables <- function(...){
  
  args <- list(...)
  
  coefs <- lapply(args,"[[","coefficients")
  coefs <- do.call("c",coefs)
  
  nms <- names(coefs)
  if(length(unique(nms))<length(nms)) {
    warning("duplicate model names, dropping previous duplicates")
    keep <- !rev(duplicated(rev(nms)))
    coefs <- coefs[keep]
    }
  else keep <- rep(TRUE,length(nms))
    
  rown <- lapply(args,rownames)
  rown <- unique(unlist(rown))
  
  coefs <- lapply(coefs,coefxpand,rown)
  
  smrys <- lapply(args, "[[","summaries")
  s.rown <- lapply(smrys,rownames)
  s.rown <- unique(unlist(s.rown))
  smrys <- lapply(smrys,smryxpand,s.rown)
  smrys <- do.call("cbind",smrys)[,keep]
  
  calls <- lapply(args,"[[","calls")
  calls <- do.call("c",calls)[keep]
  
  structure(list(
    coefficients=coefs,
    summaries=smrys,
    calls=calls),
    class="mtable")
}

c.mtable <- function(...) combine_mtables(...)
