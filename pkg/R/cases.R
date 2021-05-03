cases <- function(...,check.xor=c("warn","stop","ignore"),
                  .default=NA,.complete=FALSE,
                  check.na=c("warn","stop","ignore"),
                  na.rm=TRUE){
  subst <- match.call(expand.dots=FALSE)$...
  
  if(!missing(check.xor))  
    if(is.logical(check.xor))
      check.xor <- ifelse(check.xor,"stop","ignore")  
    else 
      check.xor <- as.character(check.xor)
  if(!missing(check.na))  
    if(is.logical(check.na))
      check.na <- ifelse(check.na,"stop","ignore")  
    else 
      check.na <- as.character(check.na)
  
  check.xor <- match.arg(check.xor)
  check.na <- match.arg(check.na)
  
  deflabels <- sapply(subst,deparse)
  if(length(subst)<2) stop("need at least two conditions")

  have.arrows <- sapply(subst,length) > 1
  have.arrows[have.arrows] <- have.arrows[have.arrows] & sapply(sapply(subst[have.arrows],"[[",1),paste)=="<-"

  parent <- parent.frame()
  
  if(all(have.arrows)){
    cond.names <- names(subst)

    conditions <- lapply(subst,"[[",3)
    values <- lapply(subst,"[[",2)
    conditions <- do.call(cbind,lapply(conditions,eval,envir=parent))
    
    if(ncol(conditions)!=length(subst)) stop("at least one condition results in NULL")
    if(!is.logical(conditions)) stop("all conditions have to be logical")
    if(any(is.na(conditions)) && check.na!="ignore") {
      msg <- switch(check.na,warn=warning,stop=stop)
      msg("At least one logical condition results in missing values")
    }
    na.cond <- is.na(conditions)
    na.count <- rowSums(na.cond)
    any.na <- na.count > 0
    all.na <- na.count == ncol(conditions)

    done <- rowSums(conditions, na.rm=TRUE)
    if((any(done > 1) || any(done >= 1 & any.na)) && check.xor!="ignore") {
      msg <- switch(check.xor,warn=warning,stop=stop)
      msg("Conditions are not mutually exclusive")
    }
    never <- colSums(conditions, na.rm=TRUE) == 0
    if(any(never) && check.xor!="ignore"){
      neverlab <- deflabels[never]
      if(length(neverlab)==1)
        warning("condition ",neverlab," is never satisfied")
      else
        warning("conditions ",paste(neverlab,collapse=", ")," are never satisfied")
    }

    values <- lapply(values,eval,envir=parent.frame(),enclos=parent.frame())
    nrow <- unique(sapply(values,length))
    if(length(nrow) > 1 || nrow != nrow(conditions)){
      nrow <- nrow(conditions)
      values <- lapply(values,function(x){
        tmp <- x
        length(tmp) <- nrow
        tmp[] <- x
        tmp
        })
    }
    values <- do.call(cbind,values)
    na_ <- as.vector(NA,mode=storage.mode(values))
    res <- vector(nrow(conditions),mode=storage.mode(values))
    res[] <- na_
    for(i in rev(1:ncol(conditions))){
      cond.i <- which(conditions[,i])
      res[cond.i] <- values[cond.i,i]
    }
    if(any(done == 0) && !is.na(.default)){
        if(length(.default) > 1) warning("only first element of '.default' used")
        if(length(.default) < 1) stop("'.default' must have non-zero length")
      xvalue <- .default[1]
      xvalue <- as.vector(xvalue,mode=storage.mode(values))
      res[done == 0 &!any.na] <- xvalue
      done <- done + 1
    }
    if(!na.rm){
       res[any.na] <- na_
    }
    nNA <- sum(is.na(res))
    if(nNA > 0) warning(nNA," NAs created")
    if(length(cond.names) && all(nzchar(cond.names))){
        uq.values <- drop(unique(values))
        if(length(uq.values)==length(cond.names))
          labels(res) <- structure(unique(uq.values),names=cond.names)
      }
    res
  }
  else if(!any(have.arrows))
  {
    conditions <- cbind(...)
    if(ncol(conditions)!=length(subst)) stop("at least one condition results in NULL")
    if(!is.logical(conditions)) stop("all conditions have to be logical")
    if(any(is.na(conditions)) && check.na!="ignore") {
      msg <- switch(check.na,warn=warning,stop=stop)
      msg("At least one logical condition results in missing values")
    }
    na.cond <- is.na(conditions)
    na.count <- rowSums(na.cond)
    any.na <- na.count > 0
    all.na <- na.count == ncol(conditions)

    codes <- 1:ncol(conditions)
    labels <- colnames(conditions)
    if(length(labels))
      labels <- ifelse(nzchar(labels),labels,deflabels)
    else labels <- deflabels

    done <- rowSums(conditions,na.rm=TRUE)
    if((any(done > 1) || any(done >= 1 & any.na)) && check.xor!="ignore") {
      msg <- switch(check.xor,warn=warning,stop=stop)
      msg("conditions are not mutually exclusive")
    }

    never <- colSums(conditions, na.rm=TRUE) == 0
    if(any(never) && check.xor!="ignore"){
      neverlab <- deflabels[never]
      if(length(neverlab)==1)
        warning("condition ",neverlab," is never satisfied")
      else
        warning("conditions ",paste(neverlab,collapse=", ")," are never satisfied")
    }

    res <- integer(nrow(conditions))
    res[] <- NA_integer_

    for(i in rev(1:ncol(conditions))){
      cond.i <- which(conditions[,i])
      res[cond.i] <- i
    }
    if(any(done == 0 & !all.na) && .complete){
      nms <- names(subst)
      if(length(nms)){
        elevel <- paste(nms,collapse="|")
      }
      else {
        elevel <- sapply(subst,deparse)
        elevel <- paste(elevel,collapse=" | ")
      }
      elevel <- paste0("!(",elevel,")")
      xcode <- length(codes) + 1
      res[done == 0 &!any.na] <- xcode
      codes <- c(codes,xcode)
      labels <- c(labels,elevel)
      done <- done + 1
    }
    if(!na.rm)
        res[any.na] <- NA_integer_
    nNA <- sum(is.na(res))
    if(nNA > 0) warning(nNA," NAs created")
    factor(res,levels=codes,labels=labels)
  }
  else stop("inconsistent arguments to 'cases'")
}
