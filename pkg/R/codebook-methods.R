setMethod("codebook","data.set",function(x){
  cb <- lapply(x,codebookEntry)
  new("codebook",cb)
})

setMethod("codebook","item",function(x){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebook","data.frame",function(x){
  cb <- lapply(x,codebookEntry)
  new("codebook",cb)
})

setMethod("codebook","atomic",function(x){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebook","factor",function(x){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebookEntry","item",function(x){
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
  new("codebookEntry",
    spec = spec,
    stats = switch(measurement(x),
            nominal=,ordinal=codebookStatsCateg(x),
            interval=,ratio=codebookStatsMetric(x)
            ),
    annotation = annotation
  )
})

setMethod("codebookEntry","atomic",function(x){
  spec <- c(
            "Storage mode:"=storage.mode(x)
            )
  isna <- is.na(x)
  stats <- summary(x)
  stats.na <- names(stats) == "NA's"
  stats <- stats[!stats.na]
  NAs <- sum(isna)
  N <- length(x)
  if(NAs> 0) {
      tab <- cbind(N=NAs,Percent=100*NAs/N)
      rownames(tab) <- "NA"
      attr(tab,"title") <- " "
  } else
      tab <- integer(0)
  stats <- list(tab=tab,
                descr=stats)

  
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = NULL
  )
})

setMethod("codebookEntry","factor",function(x){

  spec <- c("Storage mode:"=storage.mode(x))
  spec <- if(is.ordered(x)) c(spec,
            "Ordered factor with"=paste(nlevels(x),"levels"))
          else c(spec,
            "Factor with"=paste(nlevels(x),"levels"))
            
  isna <- is.na(x)
  NAs <- sum(isna)

  counts <- table(x)
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
    }
  else {
      labs <- sQuote(levels(x))
      labs <- paste(format(1:nlevels(x),justify="right"),
                    format(labs,justify="left")
                    )
  }
  rownames(tab) <- labs
  
  attr(tab,"title") <- "Levels and labels"
  stats <- list(tab=tab)
  stats <- c(stats,list(NAs = NAs))
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = NULL
  )
})

setMethod("codebookEntry","character",function(x){

  spec <- c("Storage mode:"=storage.mode(x))
  stats <- codebookStatsChar(x)
    
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = NULL
  )
})


codebookStatsCateg <- function(x){
    vl <- labels(x)
    ic <- inherits(x,"character")
    if(length(vl) || !ic)
        list(tab=codebookTable(x))
    else
        codebookStatsChar(x)
}

codebookStatsChar <- function(x){
  isna <- is.na(x)
  NAs <- sum(isna)
  descr <- structure(range(x),names=c("Min","Max"))
  list(descr=descr,
       NAs = NAs)
}

codebookStatsMetric <- function(x){
  tab <- codebookTable(x,drop.unlabelled=TRUE)
  descr <- Descriptives(x)
  list(
    tab=tab,
    descr=descr
    )
}

codebookTable <- function(x,drop.unlabelled=FALSE){
    is.m <- is.missing(x)
    isNA <- is.na(x)
    vl <- labels(x)
    if(length(vl)){
        vvl <- vl@values
        lvl <- vl@.Data
        valid <- !is.missing2(vvl,x@value.filter)
        i <- match(x@.Data,vvl,nomatch=0L)
        tab <- tabulate(i,nbins=length(vvl))
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

    ovld <- sum(!is.m & !i)
    omiss <- sum(is.m & !i & !isNA)
    NAs <- sum(isNA)

    if(ovld){
        tab <- c(tab," "=ovld)
        lab <- c(lab,"(unlab.val.)")
        valid <- c(valid,TRUE)
    }

    if(omiss){
        tab <- c(tab," "=omiss)
        lab <- c(lab,"(unlab.mss.)")
        valid <- c(valid,FALSE)
    }
    if(NAs){
        tab <- c(tab,"NA"=NAs)
        lab <- c(lab,"")
        valid <- c(valid,FALSE)
    }
    
    missing.marker <- "M"
    valid.marker <- paste(rep(" ",nchar(missing.marker)),collapse="")
    lab <- paste(ifelse(valid,valid.marker,missing.marker),lab)
    tab.nonzero <- tab>0
    tab <- tab[valid | tab.nonzero]
    lab <- lab[valid | tab.nonzero]
    valid <- valid[valid | tab.nonzero]
    if(NAs + omiss > 0){
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
        tab <- tab[-drp,,drop=FALSE]
        lab <- lab[-drp]
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
  out <- mapply(format,x=x,name=names(x),toprule=toprule,midrule=midrule)
  unlist(out)
})

setMethod("show","codebook",function(object){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=object,name=names(object),toprule=toprule,midrule=midrule)
  out <- unlist(out)
  writeLines(out)
})


Write.codebook <- function(x,file=stdout(),...){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=x,name=names(x),toprule=toprule,midrule=midrule)
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
    if(ncol(tab)>2){
    tab <- cbind(
        formatC(tab[,1,drop=FALSE],format="d"),
        formatC(tab[,2],format="f",digits=1),
        formatC(tab[,3],format="f",digits=1)
      )
    }
    else {
      tab <- cbind(
        formatC(tab[,1,drop=FALSE],format="d"),
        formatC(tab[,2],format="f",digits=1),
        ""
      )
    }
    tab[tab=="NA"] <- ""
    tab <- format(tab,justify="right")
    tab <- cbind(
        format(c(tab.title,"",rownames(tab)),justify="right"),
        format(c("N","",tab[,1]),justify="right"),
        " ",
        format(c("Percent","",apply(tab[,2:3,drop=FALSE],1,paste,collapse=" ")),justify="centre")
    )
    
    tab <- paste("  ",apply(tab,1,paste,collapse=" "))
  }
  if(length(descr)){
    descr <- cbind(
            format(paste(names(descr),": ",sep=""),justify="right"),
            format(formatC(descr,format="f",digits=3),justify="right")
            )
    descr <- paste("  ",apply(descr,1,paste,collapse=" "))
  }
  if(length(tab) && length(descr)){
    statstab <- paste("   ",format(c(tab,"",descr),justify="left"))
  }
  else if(length(tab)){
    statstab <- tab
  }
  else if(length(descr)){
    statstab <- paste("   ",descr)
  }
  
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

