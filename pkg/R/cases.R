cases <- function(...,check.xor=c("warn","stop","ignore"),
                  .default=NA,.complete=FALSE){
  subst <- match.call(expand.dots=FALSE)$...
  
  if(!missing(check.xor))  
    if(is.logical(check.xor))
      check.xor <- ifelse(check.xor,"stop","ignore")  
    else 
      check.xor <- as.character(check.xor)
  
  check.xor <- match.arg(check.xor)
    
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
    #if(any(is.na(conditions))) stop("NA in logical condition")
    na.cond <- rowSums(is.na(conditions)) > 0

    done <- rowSums(conditions, na.rm=TRUE)
    if(any(done > 1) && check.xor!="ignore") {
      msg <- switch(check.xor,warn=warning,stop=stop)
      msg("conditions are not mutually exclusive")
    }
    never <- colSums(conditions[!na.cond,,drop=FALSE]) == 0
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
    res <- vector(nrow(conditions),mode=storage.mode(values))
    conditions[na.cond,] <- FALSE
    for(i in rev(1:ncol(conditions))){
      res[conditions[,i]] <- values[conditions[,i],i]
    }
    if(any(done == 0) && !is.na(.default)){
      xvalue <- .default[1]
      xvalue <- as.vector(xvalue,mode=storage.mode(values))
      res[done == 0] <- xvalue
      done <- done + 1
    }
    res[na.cond] <- as.vector(NA,mode=storage.mode(values))
    res[done==0] <- as.vector(NA,mode=storage.mode(values))
    nNA <- sum(na.cond & done==0)
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
    #if(any(is.na(conditions))) stop("NA in logical condition")
    na.cond <- rowSums(is.na(conditions)) > 0

    codes <- 1:ncol(conditions)
    labels <- colnames(conditions)
    if(length(labels))
      labels <- ifelse(nzchar(labels),labels,deflabels)
    else labels <- deflabels

    done <- rowSums(conditions,na.rm=TRUE)
    if(any(done > 1) && check.xor!="ignore") {
      msg <- switch(check.xor,warn=warning,stop=stop)
      msg("conditions are not mutually exclusive")
    }
    never <- colSums(conditions[!na.cond,,drop=FALSE]) == 0
    if(any(never)){
      neverlab <- deflabels[never]
      if(length(neverlab)==1)
        warning("condition ",neverlab," is never satisfied")
      else
        warning("conditions ",paste(neverlab,collapse=", ")," are never satisfied")
    }

    res <- integer(nrow(conditions))
    conditions[na.cond,] <- FALSE
    for(i in rev(1:ncol(conditions))){
      res[conditions[,i]] <- i
    }
    if(any(done == 0) && .complete){
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
      res[done == 0] <- xcode
      codes <- c(codes,xcode)
      labels <- c(labels,elevel)
      done <- done + 1
    }
    res[na.cond] <- NA_integer_
    res[done==0] <- NA_integer_
    nNA <- sum(na.cond & done==0)
    if(nNA > 0) warning(nNA," NAs created")
    factor(res,levels=codes,labels=labels)
  }
  else stop("inconsistent arguments to 'cases'")
}
