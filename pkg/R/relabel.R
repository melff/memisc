relabel <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  if(isS4(x)) relabel4(x,...,gsub=gsub,fixed=fixed,warn=warn)
  else UseMethod("relabel")
}

relabel.default <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  if(!is.null(attr(x,"labels"))) labels <- attr(x,"labels")
  else labels <- names(x)
  subst <- c(...)
  if(gsub){
    for(i in 1:length(subst)){
      labels <- gsub(names(subst[i]),subst[i],labels,fixed=fixed)
    }
  }
  else {
    i <- match(names(subst),labels)
    if(any(is.na(i))) {
      if(warn) warning("undefined label(s) selected")
      if(any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if(length(i))
      labels[i] <- subst
  }
  if(!is.null(attr(x,"labels"))) attr(x,"labels") <- labels
  else names(x) <- labels
  return(x)
}

relabel.factor <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  subst <- c(...)  
  labels <- levels(x)
  if(gsub){
    for(i in 1:length(subst)){
      labels <- gsub(names(subst[i]),subst[i],labels,fixed=fixed)
    }
  }
  else {
    i <- match(names(subst),labels)
    if(any(is.na(i))) {
      if(warn) warning("undefined label(s) selected")
      if(any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if(length(i))
      labels[i] <- subst
  }
  levels(x) <- labels
  return(x)
}

relabel1 <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  subst <- c(...)
  if(gsub){
    for(i in 1:length(subst)){
      x <- gsub(names(subst[i]),subst[i],x,fixed=fixed)
    }
  }
  else {
    i <- match(names(subst),x)
    if(any(is.na(i))) {
      if(warn) warning("unused name(s) selected")
      if(any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if(length(i))
      x[i] <- subst
  }
  return(x)
}


relabel.table <- function(x,...,gsub=FALSE,fixed=TRUE,warn=FALSE){
  
  dn <- dimnames(x)
  ndn <- names(dn)
  dn <- lapply(dn,relabel1,...,gsub=gsub,fixed=fixed,warn=warn)
  ndn <- relabel1(ndn,...,gsub=gsub,fixed=fixed,warn=warn)
  names(dn) <- ndn
  dimnames(x) <- dn
  return(x)
}


relabel.ftable <- function(x,...,gsub=FALSE,fixed=TRUE,warn=FALSE){
  attr(x,"row.vars") <- relabel1ft(attr(x,"row.vars"),...,gsub=gsub,fixed=fixed,warn=warn)
  attr(x,"col.vars") <- relabel1ft(attr(x,"col.vars"),...,gsub=gsub,fixed=fixed,warn=warn)
  return(x)
}

relabel1ft <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  n.x <- names(x)
  if(length(n.x))
    n.x <- relabel1(n.x,...,gsub=gsub,fixed=fixed,warn=warn)
  x <- relabel1(x,...,gsub=gsub,fixed=fixed,warn=warn)
  names(x) <- n.x
  return(x)
}

relabel.ftable_matrix <- function(x,...,gsub=FALSE,fixed=TRUE,warn=FALSE){
  attr(x,"row.vars") <- lapply(attr(x,"row.vars"),relabel1ft,...,gsub=gsub,fixed=fixed,warn=warn)
  attr(x,"col.vars") <- lapply(attr(x,"col.vars"),relabel1ft,...,gsub=gsub,fixed=fixed,warn=warn)
  return(x)
}
