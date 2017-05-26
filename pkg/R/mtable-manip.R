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

}



##

combine_mtables <- function(...){
  
  args <- list(...)
  argnames <- names(args)
  

}

c.mtable <- function(...) combine_mtables(...)
