percentages <- function(obj,...) UseMethod("percentages")

percentages.table <- function(obj,by=NULL,which=NULL,se=FALSE,ci=FALSE,ci.level=.95,...){
  
  dn <- dimnames(obj)
  ndn <- names(dn)
  if(!length(ndn) || !any(nzchar(ndn))) {
    ndn <- as.character(seq_along(dn))
  }

  by <- unique(by)
  which <- unique(which)
  
  lby <- length(by)
  lwi <- length(which)
  
  if(!(lby||lwi)){
    margin <- NULL
  } else if (lby && lwi) {
    
    if(length(intersect(by,which))>0)
      stop("duplicate variables")
    
    all <- union(which,by)
    if(is.numeric(all)) {
      ii <- all
    }
    else {
      ii <- match(all,ndn)
    }
    if(any(is.na(ii))) stop("undefined variables")
    if(length(ii)<length(dn)){
      obj <- margin.table(obj,ii)
      return(Recall(obj,by=by,which=which,se=se,ci=ci,ci.level=ci.level,...))
    }
    else {
      margin <- match(by,ndn)
    }
  }
  else if(lby){
    if(is.numeric(by)) {
      margin <- by
    }
    else {
      margin <- match(by,ndn)
      if(any(is.na(margin))) stop("undefined variables")
    }
  }
  else if(lwi){
    if(is.numeric(which)) {
      margin <- setdiff(seq_along(dn),which)
    }
    else {
      margin <- match(setdiff(ndn,which),ndn)
      if(any(is.na(margin))) stop("undefined variables")
    }
  }
  
  tab <- obj  # Just another name ...
  ptab <- prop.table(tab,margin=margin)
  if(!ci && !se){
    structure(100*ptab,class=c("percentage.table",class(obj)))
  } else {
    mtab <- margin.table(tab,margin=margin)
    
    dd <- seq_along(dim(ptab))
    
    perm <- c(margin,setdiff(dd,margin))
    rev.perm <- seq_along(perm)
    rev.perm[perm] <- rev.perm
    
    mtab <- aperm(array(mtab,dim=dim(ptab)[perm]),
                  rev.perm)
    dimnames(mtab) <- dimnames(ptab)
    if(se){
      var.tab <- ptab*(1-ptab)/mtab
      se.tab <- sqrt(var.tab)
    }
    if(ci){
      alpha <- (1-ci.level)/2
      lower <- upper <- array(NA,dim=dim(ptab))
      isnull <- ptab == 0 | is.na(ptab)
      isfull <- ptab == 1 | is.na(ptab)
      lower[!isnull] <- qbeta(alpha,tab[!isnull],mtab[!isnull]-tab[!isnull]+1)
      lower[isnull] <- 0
      upper[!isfull] <- qbeta(1-alpha,tab[!isfull]+1,mtab[!isfull]-tab[!isfull])
      upper[isfull] <- 1
    }
    if(!ci){
      res <- 100*cbind(as.vector(ptab),as.vector(se.tab))
      res <- array(res,dim=c(dim(ptab),2),
                   dimnames=c(
                     dimnames(ptab),
                     list(Result=c("Percentage","SE"))
                     ))
    }
    else if(!se){
      res <- 100*cbind(as.vector(ptab),as.vector(lower),as.vector(upper))
      res <- array(res,dim=c(dim(ptab),3),
                   dimnames=c(
                     dimnames(ptab),
                     list(Result=c("Percentage","Lower bound","Upper bound"))
                   ))
    }
    else {
      res <- 100*cbind(as.vector(ptab),as.vector(se.tab),
                       as.vector(lower),as.vector(upper))
      res <- array(res,dim=c(dim(ptab),4),
                   dimnames=c(
                     dimnames(ptab),
                     list(Result=c("Percentage","SE","Lower bound","Upper bound"))
                   ))
    }
    structure(res,class=c("xpercentage.table","percentage.table",class(obj)))
  }
}

percentages.formula <- function(obj,data=parent.frame(),weights=NULL,...){
  
  if(is.table(data))
    tab <- data
  else{
    
    allv <- all.vars(obj)
    allv.formula <- obj
    if(length(obj)==3)
      allv.formula <- reformulate(allv)    
    else 
      allv.formula <- obj
    
    if(!missing(weights)){
          weights <- deparse(substitute(weights))
          allv.formula <- reformulate(all.vars(allv.formula),
                                      response=weights)
    }
    my.call <- match.call()
    xtc <- my.call
    ii <- match(names(formals(stats::xtabs)),names(xtc),0L)
    xtc <- xtc[c(1L,ii)]
    xtc[[1L]] <- quote(stats::xtabs)
    xtc$formula <- allv.formula
    if(is.environment(data)){
        mf <- model.frame(allv.formula,data=data)
        xtc$data <- mf
    } else if(is.data.set(data)) {
      data <- as.data.frame(data[allv])
    } else if(is.data.frame(data)) {
      data <- data[allv]
    }
    tab <- eval(xtc,parent.frame())
  }
  
  if(length(obj)==3){
    which <- all.vars(obj[-3])
    by <- all.vars(obj[-2])
  }
  else {
    which <- all.vars(obj)
    by <- NULL
  }
  percentages.table(tab,by=by,which=which,...)
}

percentages.default <- function(obj,weights=NULL,...){
    lab <- deparse(substitute(obj))
    percentages1(obj,weights=weights,name=lab,...)
}

percentages.list <- function(obj,weights=NULL,...){
    nms <- names(obj)
    if(!length(nms))
        nms <- rep("",length(obj))
    mapply(percentages1,obj,nms,MoreArgs=list(weights=weights,...))
}

percentages.data.frame <- percentages.list

percentages1 <- function(x,weights=NULL,name,...){
    if(length(weights))
    {
        tab <- rowsum(weights,x)
        dnm <- dimnames(tab)
        dim(tab) <- dim(tab)[1]
        dimnames(tab) <- dnm[1]
        class(tab) <- "table"
    }
    else
        tab <- table(x)
    tab <- percentages.table(tab,...)
    names(dimnames(tab))[1] <- name
    tab
}


as.data.frame.percentage.table <- function(x,...){
  res <- NextMethod("as.data.frame")
  rename(res,Freq="Percentage")
}

as.data.frame.xpercentage.table <- function(x,...){
  dx <- dim(x)
  dn <- dimnames(x)
  ld <- length(dx)
  lst.d <- dx[ld]
  xm <- array(x,dim=c(prod(dx[-ld]),dx[ld]))
  tabs <- lapply(1:dx[ld],mcol2df,
                 x=xm,
                 dim=dx[-ld],
                 dimnames=dn[-ld],
                 slicenames=dn[[ld]]
                 )
  Freq.col <- which("Freq"==names(tabs[[1]]))
  result.names <- dimnames(x)$Result
  for(i in seq_along(tabs))
      names(tabs[[i]])[Freq.col] <- result.names[i]
  Reduce(merge,tabs)
}

mcol2df <- function(i,x,dim,dimnames,slicenames){
  y <- structure(x[,i],
                 class="table",
                 dim=dim,
                 dimnames=dimnames)
  y <- as.data.frame(y)
  rename(y,Freq=slicenames[i])
}
  
