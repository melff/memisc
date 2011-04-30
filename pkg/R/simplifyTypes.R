
simplifyTypes <- function(x,verbose=TRUE,...){
  if(!is.data.frame(x) && !is.data.set(x)) stop("first arg. should be a data.frame or data.set")
  attr.x <- attributes(x)
  y <- list()
  for(i in seq_along(x)){
    y[[i]] <- simplifyType(x[[i]])
    if(verbose && typeof(y[[i]])!=typeof(x[[i]])) message("Changed type of variable ",
                                  names(x)[i],
                                  " from ",
                                  sQuote(typeof(x[[i]])),
                                  " to ",
                                  sQuote(typeof(y[[i]])))
    }
  attributes(y) <- attr.x
  y
}

simplifyType <- function(x) UseMethod("simplifyType")
simplifyType.default <- function(x) x
simplifyType.double <- function(x){
  ok <- is.finite(x)
  #y <- as.integer(x)
  if(all(x[ok]==trunc(x[ok]))) as.integer(x) else x
}
simplifyType.labelled <- function(x){
  if(typeof(x) %in% c("integer","character")) return(x)
  ok <- is.finite(x)
  if(any(x[ok]!=trunc(x[ok]))) return(x)
  y <- x
  attributes(y) <- NULL
  y <- as.integer(y)
  attributes(y) <- attributes(x)
  if(length(cds <- attr(y,"labels"))){
    clab <- names(cds)
    cds <- as.integer(cds)
    names(cds) <- clab
    attr(y,"labels") <- cds
    }
  if(length(msg <- attr(y,"missing.values"))){
    if(length(msg$values)) msg$values <- as.integer(msg$values)
    #if(length(msg$range)) msg$range[is.finite(msg$range)] <- as.integer(msg$range)
    attr(y,"missing.values") <- msg
    }
  y
}
