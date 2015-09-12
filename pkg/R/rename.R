rename <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  subst <- c(...)
  if(gsub){
    names.x <- names(x)
    for(i in 1:length(subst)){
      names.x <- gsub(names(subst[i]),subst[i],names.x,fixed=fixed)
    }
    names(x) <- names.x
  }
  else {
    i <- match(names(subst),names(x))
    if(any(is.na(i))) {
      if(warn) warning("unused name(s) selected")
      if(any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if(length(i))
      names(x)[i] <- subst
  }
  return(x)
}

dimrename <- function(x,dim=1,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  subst <- c(...)
  if(0 %in% dim){
    dimnames(x) <- rename(dimnames(x),...,gsub=gsub,fixed=fixed,warn=warn)
    dim <- dim[dim!=0]
  }
  if(length(dim)){
    for(i in 1:length(subst)){
      for(j in dim){
          if(gsub)
            dimnames(x)[[j]] <- gsub(names(subst[i]),subst[i],dimnames(x)[[j]],fixed=fixed)
          else{
            ii <- match(names(subst[i]),dimnames(x)[[j]])
            if(any(is.na(ii))) {
              if(warn) warning("unused dimname(s) selected")
              if(any(!is.na(ii)))
                subst[i] <- subst[!is.na(ii)]
              ii <- ii[!is.na(ii)]
            }
            if(length(ii))
              dimnames(x)[[j]][ii] <- subst[i]
          }
       }
    }
  }
  return(x)
}

colrename <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE)
  dimrename(x,dim=2,...,gsub=gsub,fixed=fixed,warn=warn)

rowrename <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE)
  dimrename(x,dim=1,...,gsub=gsub,fixed=fixed,warn=warn)

