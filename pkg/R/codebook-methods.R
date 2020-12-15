setMethod("codebook","data.set",function(x,weights,unweighted=TRUE,...){
  weights <- eval(substitute(weights),x,parent.frame())
  cb <- lapply(x,codebookEntry,weights=weights,unweighted=unweighted)
  new("codebook",cb)
})

setMethod("codebook","item",function(x,weights,unweighted=TRUE,...){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x,weights=weights,unweighted=unweighted))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebook","data.frame",function(x,weights,unweighted=TRUE,...){
  weights <- eval(substitute(weights),x,parent.frame())
  cb <- lapply(x,codebookEntry,weights=weights,unweighted=unweighted)
  new("codebook",cb)
})

setMethod("codebook","atomic",function(x,weights,unweighted=TRUE,...){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x,weights=weights,unweighted=unweighted))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebook","factor",function(x,weights,unweighted=TRUE,...){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x,weights=weights,unweighted=unweighted))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebook","ANY",function(x,weights,unweighted=TRUE,...){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x,weights=weights,unweighted=unweighted))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebook","NULL",function(x,weights,unweighted=TRUE,...)NULL)

setMethod("codebookEntry","item",function(x,weights,unweighted=TRUE,...){
  annotation <- annotation(x)
  filter <- x@value.filter
  spec <- c(
            "Storage mode:"=storage.mode(x),
            "Measurement:"=measurement(x)
            )
  if(length(filter)) spec <- c(spec,
                              switch(class(filter),
                                        missing.values = c("Missing values:" = format(filter)),
                                        valid.values   = c("Valid values:"   = format(filter)),
                                        valid.range    = c("Valid range:"    = format(filter))
                                     ))
  stats <- switch(measurement(x),
            nominal=,ordinal=codebookStatsCateg(x,weights=weights,unweighted=unweighted),
            interval=,ratio=codebookStatsMetric(x,weights=weights,unweighted=unweighted)
            )
  cbe <- new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = annotation
    )
  return(cbe)
})

NAtab <- function(isna,weights=NULL){
  if(!length(weights))
    weights <- rep(1,length(isna))
  counts <- c(
    Valid = sum(weights*!isna),
    "Missing (NA)" = sum(weights*isna),
    Total = sum(weights)
  )
  perc <- 100*counts/counts[3]
  perc[3] <- NA
  cbind(N=counts,
        Percent=perc)
}

setMethod("codebookEntry","atomic",function(x,weights,unweighted=TRUE,...){

  if(length(attr(x,"label")))
      annotation <- c(description=attr(x,"label"))
  else
      annotation <- NULL

  spec <- c("Storage mode:"=storage.mode(x))
  isna <- is.na(x)
  descr <- Descriptives(x)
  if(length(weights) && length(descr) > 2){ # There is more than a range
      wdescr <- Descriptives(x,weights)
      if(unweighted)
          descr <- collect(Unweighted=descr,
                           Weighted=wdescr)
      else
          descr <- as.matrix(wdescr)
  }
  else 
      descr <- as.matrix(descr)

  if(any(isna)){
    tab <- NAtab(isna)
    if(length(weights)){
      wtab <- NAtab(isna,weights)
      if(unweighted)
          tab <- collect(Unweighted=tab,
                         Weighted=wtab)
      else
          tab <- array(wtab,
                       dim=c(dim(tab),1),
                       dimnames=c(dimnames(tab),
                                  list(NULL)))
    }
    else
      tab <- array(tab,
                   dim=c(dim(tab),1),
                   dimnames=c(dimnames(tab),
                              list(NULL)))
    attr(tab,"title") <- "Valid and missing values"
  } else
    tab <- integer(0)

  stats <- list(tab=tab,
                descr=descr)
  
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = annotation
  )
})


setMethod("codebookEntry","ANY",function(x,weights,unweighted=TRUE,...){

  if(length(attr(x,"label")))
      annotation <- c(description=attr(x,"label"))
  else
      annotation <- NULL

  spec <- c("Storage mode:"=storage.mode(x))

  isat <- is.atomic(x)
  if(isat)
    isna <- is.na(x)
  else
    isna <- FALSE

  if(mode(x) == "numeric"){
    descr <- Descriptives(x)
    if(length(weights) && length(descr) > 2){ # There is more than a range
      wdescr <- Descriptives(x,weights)
      if(unweighted)
          descr <- collect(Unweighted=format(descr),
                           Weighted=format(wdescr))
      else
          descr <- as.matrix(format(wdescr))
    }
    else 
      descr <- as.matrix(format(descr))
  }
  else
    descr <- NULL

  if(any(isna)){
    tab <- NAtab(isna)
    if(length(weights)){
      wtab <- NAtab(isna,weights)
      if(unweighted)
          tab <- collect(Unweighted=tab,
                         Weighted=wtab)
      else
          tab <- array(wtab,
                       dim=c(dim(tab),1),
                       dimnames=c(dimnames(tab),
                                  list(NULL)))
    }
    else
      tab <- array(tab,
                   dim=c(dim(tab),1),
                   dimnames=c(dimnames(tab),
                              list(NULL)))
    attr(tab,"title") <- "Valid and missing values"
  } else
    tab <- integer(0)

  stats <- list(tab=tab,
                descr=descr)
  
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = annotation
  )
})

rwnexp <- function(mat,nms){
  res <- array(0,c(length(nms),ncol(mat)),
               dimnames=list(nms,colnames(mat)))
  rn <- rownames(mat)
  res[rn,] <- mat
  return(res)
}

codebookTable_factor <- function(x,weights=NULL,...){

  if(!length(weights))
    weights <- rep(1,length(x))

  isna <- is.na(x)
  counts <- rowsum(weights[!isna],x[!isna])
  lev <- levels(x)
  counts <- rwnexp(counts,lev)
  
  NAs <- sum(weights*isna)

  tab <- cbind(counts,100*counts/sum(counts))
  if(any(isna)) {
      labs <- sQuote(levels(x))
      if(nlevels(x))
        labs <- paste(format(c(1:nlevels(x),NA),justify="right"),
                      format(c(labs,""),justify="left")
                      )
      else
        labs <- "NA"
      tab <- rbind(tab,c(NAs,NA))
      counts <- c(counts,NAs)
      tab <- cbind(tab,100*counts/sum(counts))
      colnames(tab) <- c("N","Valid","Total")
    }
  else {
      labs <- sQuote(levels(x))
      labs <- paste(format(1:nlevels(x),justify="right"),
                    format(labs,justify="left")
                    )
      colnames(tab) <- c("N","Valid")
  }
  rownames(tab) <- labs
  

  return(tab)
}

setMethod("codebookEntry","factor",function(x,weights=NULL,unweighted=TRUE,...){

  tab <- codebookTable_factor(x)
  if(length(weights)){
    wtab <- codebookTable_factor(x,weights=weights)
    if(unweighted)
        tab <- collect(Unweighted=tab,
                       Weighted=wtab)
    else {
        tab <- wtab
        tab.dn <- dimnames(tab)
        tab.d <- dim(tab)
        dim(tab) <- c(tab.d,1)
        dimnames(tab) <- c(tab.dn,list(NULL))
    }
  }
  else{
    tab.dn <- dimnames(tab)
    tab.d <- dim(tab)
    dim(tab) <- c(tab.d,1)
    dimnames(tab) <- c(tab.dn,list(NULL))
  }
  attr(tab,"title") <- "Levels and labels"
  
  if(length(attr(x,"label")))
      annotation <- c(description=attr(x,"label"))
  else
      annotation <- NULL
    
  spec <- c("Storage mode:"=storage.mode(x))
  spec <- if(is.ordered(x)) c(spec,
            "Ordered factor with"=paste(nlevels(x),"levels"))
          else c(spec,
            "Factor with"=paste(nlevels(x),"levels"))
            
  stats <- list(tab=tab)
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = annotation
  )
})

setMethod("codebookEntry","character",function(x,weights=NULL,unweighted=TRUE,...){
  spec <- c("Storage mode:"=storage.mode(x))
  isna <- is.na(x)
  descr <- codebookStatsChar(x)
  descr <- descr$descr

  if(any(isna)){
    tab <- NAtab(isna)
    if(length(weights)){
      wtab <- NAtab(isna,weights)
      if(unweighted)
          tab <- collect(Unweighted=tab,
                         Weighted=tab)
      else
          tab <- array(wtab,
                       dim=c(dim(tab),1),
                       dimnames=c(dimnames(tab),
                                  list(NULL)))
    }
    else
      tab <- array(tab,
                   dim=c(dim(tab),1),
                   dimnames=c(dimnames(tab),
                              list(NULL)))
    attr(tab,"title") <- "Valid and missing values"
  } else
    tab <- integer(0)

  stats <- list(tab=tab,
                descr=descr)
  
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = NULL
  )
})


codebookStatsCateg <- function(x,weights=NULL,unweighted=TRUE,...){
    vl <- labels(x)
    ic <- inherits(x,"character")
    if(length(vl) || !ic){
        tab <- codebookTable_item(x)
        tab.title <- attr(tab,"title")
        if(length(weights)){
            wtab <- codebookTable_item(x,weights)
            if(unweighted)
                tab <- collect(Unweighted=tab,
                               Weighted=wtab)
            else
                tab <- array(wtab,
                             dim=c(dim(tab),1),
                             dimnames=c(dimnames(tab),
                                        list(NULL)))
        }
        else
            tab <- array(tab,
                         dim=c(dim(tab),1),
                         dimnames=c(dimnames(tab),
                                    list(NULL)))
        attr(tab,"title") <- tab.title
        list(tab=tab)
    }
    else
        codebookStatsChar(x)
}

codebookStatsChar <- function(x,...){
  descr <- structure(dQuote(range(x,na.rm=TRUE)),
                     names=c("Min","Max"))
  descr <- as.matrix(descr)
  list(
    tab = NULL,
    descr = descr
  )
}

codebookStatsMetric <- function(x,weights=TRUE,unweighted=TRUE,...){

  if(length(labels(x))){
    tab <- codebookTable_item(x,drop.unlabelled=TRUE)
    tab.title <- attr(tab,"title")
    if(length(weights) && length(tab)){
      wtab <- codebookTable_item(x,weights=weights,
                                 drop.unlabelled=TRUE)
      if(unweighted)
          tab <- collect(Unweighted=tab,
                         Weighted=wtab)
      else
          tab <- array(wtab,
                       dim=c(dim(tab),1),
                       dimnames=c(dimnames(tab),
                                  list(NULL)))
      attr(tab,"title") <- tab.title
    }
    else if(length(tab)){
      tab <- array(tab,
                   dim=c(dim(tab),1),
                   dimnames=c(dimnames(tab),
                              list(NULL)))
      attr(tab,"title") <- tab.title
    }
  }
  else
    tab <- NULL
  
  descr <- Descriptives(x)[1:4]
  if(length(weights)){
      wdescr <- Descriptives(x,weights)[1:4]
      if(unweighted)
          descr <- collect(Unweighted=descr,
                           Weighted=wdescr)
      else
          descr <- as.matrix(wdescr)
  }
  else 
      descr <- as.matrix(descr)
  list(
    tab=tab,
    descr=descr
    )
}

codebookTable_item <- function(x,weights=NULL,drop.unlabelled=FALSE,drop.empty=TRUE){

  is.m <- is.missing(x)
  isNA <- is.na(x)
  vl <- labels(x)
  if(!length(weights)){
    weights <- rep(1,length(x))
  }
  if(length(vl)){
    vvl <- vl@values
    lvl <- vl@.Data
    valid <- !is.missing2(vvl,x@value.filter)
    i <- match(x@.Data,vvl,nomatch=0L)
    tab <- drop(rowsum(weights,i))
    if("0" %in% names(tab)){
      ii <- match("0",names(tab))
      tab <- tab[-ii]
    }
    if(length(tab) < length(vvl)){
      tab0 <- numeric(length(vvl))
      ii <- as.integer(names(tab))
      tab0[ii] <- tab
      tab <- tab0
    }
    names(tab) <- as.character(vvl)
    lab <- sQuote(vl@.Data)
    tab.title <- "Values and labels"
  }
  else {
    valid <- logical(0)
    tab <- c()
    lab <- c()
    i <- logical(length(x))
    tab.title <- "Values"
  }

  ovld <- sum(weights*(!is.m & !i))
  omiss <- sum(weights*(is.m & !i & !isNA))
  NAs <- sum(weights*(isNA))

  if(ovld | !drop.empty){
    tab <- c(tab," "=ovld)
    lab <- c(lab,"(unlab.val.)")
    valid <- c(valid,TRUE)
  }

  if(omiss | !drop.empty){
    tab <- c(tab," "=omiss)
    lab <- c(lab,"(unlab.mss.)")
    valid <- c(valid,FALSE)
  }
  if(NAs | !drop.empty){
    tab <- c(tab,"NA"=NAs)
    lab <- c(lab,"")
    valid <- c(valid,FALSE)
  }

  if(any(!valid)){
    missing.marker <- "M"
    valid.marker <- paste(rep(" ",nchar(missing.marker)),collapse="")
    lab <- paste(ifelse(valid,valid.marker,missing.marker),lab)
  }
  tab.nonzero <- tab>0
  tab.keep <- tab.nonzero | !drop.empty
  tab <- tab[valid | tab.keep]
  lab <- lab[valid | tab.keep]
  valid <- valid[valid | tab.keep]
  if(any(!valid)){
    vperc <- rep(NA,length(tab))
    vtab <- tab[valid]
    Nvalid <- sum(vtab)
    if(Nvalid) vperc[valid] <- 100 * vtab/Nvalid
    else vperc[valid] <- 0
    tperc <- 100 * tab/sum(tab)
    tab <- cbind(N=tab,Valid=vperc,Total=tperc)
  } else {
    tperc <- 100 * tab/sum(tab)
    tab <- cbind(N=tab,Percent=tperc)
  }
  rownames(tab) <- names(tperc)
  if(drop.unlabelled){
      drp <- match("(unlab.val.)",trimws(lab),nomatch=0L)
      if(drp > 0){
          tab <- tab[-drp,,drop=FALSE]
          lab <- lab[-drp]
          
      }
    if(all(is.na(tab[,2])) && length(tab)){
        tab <- tab[,-2,drop=FALSE]
        colnames(tab) <- c("N","Percent")
    }
  }
  if(!length(tab))
    tab <- NULL
  else {
    rownames(tab) <- paste(format(rownames(tab),justify="right"),format(lab,justify="left"))
    attr(tab,"title") <- tab.title
  }
  return(tab)
}


setMethod("as.character","codebook",function(x,...){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=x@.Data,name=names(x),toprule=toprule,midrule=midrule)
  unlist(out)
})

setMethod("show","codebook",function(object){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=object@.Data,name=names(object),toprule=toprule,midrule=midrule)
  out <- unlist(out)
  writeLines(out)
})


Write.codebook <- function(x,file=stdout(),...){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=x@.Data,name=names(x),toprule=toprule,midrule=midrule)
  out <- unlist(out)
  writeLines(out,con=file)
}


setMethod("format","codebookEntry",
  function(x,name="",width=getOption("width"),
          toprule=paste(rep("=",width),collapse=""),
          midrule=paste(rep("-",width),collapse="")
      ){
      
  annot <- x@annotation
  description <- annot["description"]
  wording <- annot["wording"]
  if(length(annot)) annot <- annot[names(annot) %nin% c("description","wording")]

  title <- strwrap(if(length(description) && !is.na(description))
                  paste(name[1],sQuote(description))
                  else name[1] ,width=width,prefix="   ")
  
  wording <- if(length(wording) && !is.na(wording))
                strwrap(dQuote(wording),width=width,prefix="   ")
             else NULL

  spec <- paste("  ",names(x@spec),x@spec)
  tab <- unclass(x@stats$tab)
  descr <- unclass(x@stats$descr)
  
  if(length(tab)){

    tab.title <- attr(tab,"title")
    tab.d <- dim(tab)
    tab.dn <- dimnames(tab)
    tab <- apply(tab,3,format_cb_table)
    #tab <- lapply(tab,unlist)
    #tab <- do.call(cbind,tab)
    rn <- rownames(tab)
    rn[1] <- tab.title
    rn <- format(rn)
    if(tab.d[3]>1){
       tab <- cbind(" "=rn,tab)
       tab <- rbind(colnames(tab),"",tab)
       tab <- apply(tab,2,format,justify="right")
    }
    else {
      tab <- cbind(rn,tab)
    }
    tab <- paste("  ",apply(tab,1,paste,collapse=" "))
  }
  if(!is.matrix(descr)) descr <- NULL
  if(length(descr)){
    descr.rn <- format(paste(rownames(descr),":",sep=""),justify="right")
    if(is.numeric(descr[]))  
       descr[] <- formatC(descr[],format="f",digits=3)
    descr[] <- gsub("NA","",descr[])
    if(!length(ncol(descr))) browser()
    if(ncol(descr) > 1){
      descr.rn <- c("","",descr.rn)
      descr <- rbind(colnames(descr),"",descr)
    }
    descr <- cbind(descr.rn,descr)
    descr <- apply(descr,2,format,justify="right")
    descr <- paste("  ",apply(descr,1,paste,collapse=" "))
  }
  if(length(tab) && length(descr)){
    statstab <- format(c(tab,"",descr),justify="left")
  }
  else if(length(tab)){
    statstab <- tab
  }
  else if(length(descr)){
    statstab <- descr
  }
  else
    statstab <- NULL
  
  annot.out <- character()
  if(length(annot)){
    for(i in seq_len(length(annot))){
      annot.i <- annot[i]
      nm.i <- trimws(names(annot.i))
      annot.i <- strwrap(annot.i,width=getOption("width")-8-4)
      annot.i <- c(paste("      ",annot.i),"")
      if(nzchar(nm.i)){
        annot.i <- c(
          paste("   ",nm.i,":",sep=""),
          annot.i
          )
      }
      annot.out <- c(annot.out,annot.i)
    }
  }
  c(
    toprule,
    "",
    title,
    if(length(wording)) c(
        "",
        wording
        ),
    "",
    midrule,
    "",
    spec,
    "",
    statstab,
    "",
    if(length(annot.out)) annot.out
    )
})

format_cb_table <- function(tab){
    cn <- colnames(tab)
    if(ncol(tab)>2){
        if(all(trunc(tab[,1])==tab[,1])){
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="d"),
                formatC(tab[,2],format="f",digits=1),
                formatC(tab[,3],format="f",digits=1)
            )
        } else {
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="f",digits=1),
                formatC(tab[,2],format="f",digits=1),
                formatC(tab[,3],format="f",digits=1)
            )
        }
    }
    else {
        if(all(trunc(tab[,1])==tab[,1])){
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="d"),
                formatC(tab[,2],format="f",digits=1)
            )
        } else {
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="f",digits=1),
                formatC(tab[,2],format="f",digits=1)
            )
        }
        
    }
    tab[tab=="NA"] <- ""
    tab <- rbind(" "=cn,"",tab)
    tab <- format(tab,justify="right")
    tab <- apply(tab,1,paste,collapse=" ")
    tab
}

setMethod("[",signature(x="codebook",i="atomic",j="missing",drop="ANY"),
  function(x,i,j,...,drop=TRUE){
      if(is.character(i))
          i <- match(i,names(x))
      cb <- x@.Data[i]
      if(!length(cb)) return(NULL)
      if(is.numeric(i) || is.logical(i))
          names(cb) <- names(x)[i]
      else return(NULL)
      new("codebook",cb)
  })

setMethod("$",signature(x="codebook"),
function(x,name){
  i <- match(name,names(x))
  cb <- x@.Data[i]
  if(!length(cb)) return(NULL)
  names(cb) <- name
  new("codebook",cb)
})

setMethod("[[",signature(x="codebook"),
function(x,i,...){
      if(is.character(i))
          i <- match(i,names(x))
      cb <- x@.Data[i]
      if(!length(cb)) return(NULL)
      if(is.numeric(i) || is.logical(i))
          names(cb) <- names(x)[i]
      new("codebook",cb)
})
