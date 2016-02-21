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
    if(any(is.na(i))) stop("undefined row names")
  } 
  if(is.character(j)) {
    j <- match(j,colnms)
    if(any(is.na(j))) stop("undefined column names")
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
  argnames <- names(args)
  
  coefs <- lapply(args,"[[","coefficients")
  
  model.groups <- lapply(coefs,names)
  model.names <- unlist(model.groups)
  if(!length(argnames) || !any(nzchar(argnames)))
    model.groups <- NULL
  else {
    mg <- lapply(model.groups,seq_along)
    mg1 <- lapply(model.groups,length)
    mg1 <- cumsum(c(0,mg1[-length(mg1)]))
    model.groups <- mapply(`+`,mg,mg1,SIMPLIFY=FALSE)
  }
  
  if(all(sapply(coefs,length)==1) && length(argnames) && any(nzchar(argnames))){
    model.names[nzchar(argnames)] <- argnames[nzchar(argnames)]
    model.groups <- NULL
  }
  
  coefs <- do.call("c",coefs)
  nms <- names(coefs)
  
  rown <- lapply(args,rownames)
  rown <- unique(unlist(rown))
  
  coefs <- lapply(coefs,coefxpand,rown)
  
  smrys <- lapply(args, "[[","summaries")
  s.rown <- lapply(smrys,rownames)
  s.rown <- unique(unlist(s.rown))
  smrys <- lapply(smrys,smryxpand,s.rown)
  smrys <- do.call("cbind",smrys)
  
  names(coefs) <- model.names
  colnames(smrys) <- model.names
  
  calls <- lapply(args,"[[","calls")
  calls <- do.call("c",calls)
  
  structure(list(
    coefficients=coefs,
    summaries=smrys,
    calls=calls,
    model.groups=model.groups),
    class="mtable")
}

c.mtable <- function(...) combine_mtables(...)
