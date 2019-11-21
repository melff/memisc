setMethod("as.data.set","importer",function(x,row.names=NULL,optional=NULL,
                    compress.storage.modes=FALSE,...){
  seekData(x)
  res <- readData(x,n=nrow(x))
  if(compress.storage.modes)
    res <- lapply(res,function(x){
      x@.Data <- compress.storage.mode(x@.Data)
      cl.x <- paste(storage.mode(x@.Data),"item",sep=".")
      attr(cl.x,"package") <- attr(class(x),"package")
      class(x) <- cl.x
      x
      })
  names(res) <- names(x)
  class(res) <- "data.frame"
  if(length(row.names)){
    if(length(row.names)!=nrow(x)) stop("row.names argument has wrong length")
    attr(res,"row.names") <- row.names
  }
  else
    attr(res,"row.names") <- seq_len(nrow(x))
  new("data.set",res)
})


setMethod("dim","importer",function(x){
  c(getNobs(x),length(x@.Data))
})

# setMethod("names","importer",function(x)names(x@frame))


bracket.importer <- function(x,Nargs,i,j,drop=TRUE,compress.storage.modes=FALSE,...){
  mycall <- match.call()
  mspl <- missing(compress.storage.modes)
  mdrp <- missing(drop)
  #Nargs <- Nargs - (!mdrp) - (!mspl) - length(list(...))
  
  ncases <- nrow(x)
  names <- names(x)
  nvar <- length(names)

#   browser()
#  message("Nargs = ",Nargs)

  if(Nargs == 1){
    if(missing(i)) return(as.data.set(x))
    if(is.array(i)) stop("index arrays not supported")
    j <- i
    i <- rep(TRUE,ncases)
  }
  else{
    if(missing(i)){
      i <- rep(TRUE,ncases)
    }
    if(missing(j)){
      j <- rep(TRUE,nvar)
    }
  }
  
  if(is.logical(i)){
    rows <- i
    ui <- which(i)
  } else if(is.numeric(i)){
    i <- as.integer(i)
    ui <- sort(unique(i))
    rows <- logical(max(ui))
    rows[ui] <- TRUE
  }
  else stop("mode ",sQuote(mode(i))," not supported for row selection")

  if(is.logical(j)){
    cols <- j
  } else if(is.numeric(j)){
    if(max(j)>nvar) stop("undefined columns selected")
    cols <- logical(nvar)
    cols[j] <- TRUE
  } else if(is.character(j)){
    if(!all(j %in% names)) stop("undefined columns selected")
    cols <- names %in% j
  } else stop("mode ",sQuote(mode(i))," not supported for column selection")
  
  seekData(x)
  if(length(rows) > nrow(x)) length(rows) <- nrow(x)
  res <- readSlice(x,rows,cols)
  if(compress.storage.modes)
    res <- lapply(res,function(x){
      x@.Data <- compress.storage.mode(x@.Data)
      cl.x <- paste(storage.mode(x@.Data),"item",sep=".")
      attr(cl.x,"package") <- attr(class(x),"package")
      class(x) <- cl.x
      x
      })
  names(res) <- names[cols]
  attr(res,"row.names") <- ui
  class(res) <- "data.frame"
  if(is.numeric(i)){
    ii <- match(i,ui)
    res <- res[ii,,drop=FALSE]
  }
  if(drop && length(res)==1) res[[1]]
  else new("data.set",res)
}

setMethod("[",signature(x="importer",i="atomic",j="atomic",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)
setMethod("[",signature(x="importer",i="atomic",j="missing",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)
setMethod("[",signature(x="importer",i="missing",j="atomic",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)
setMethod("[",signature(x="importer",i="missing",j="missing",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)

c_Data <- function(x1,x2) {
  if(is.null(x1)) return(x2) 
  else {
    y <- x1
    y@.Data <- c(x1@.Data,x2@.Data)
    return(y)
  }
}

setMethod("subset","importer",
    function (x, subset, select, drop = FALSE,
              ...)
{

    if(missing(subset) && missing(select))
        return(as.data.set(x))
              
    nobs <- nrow(x)
    names <- names(x)
    nvars <- length(names)
    cs <- getOption("subset.chunk.size",nobs)
    if(cs < 1) stop("'subset.chunk.size' option must be a positive integer or NULL")
    
    if(missing(select)){
        cols <- rep(TRUE,nvars)
        select.vars <- names(x)
    }
    else {
        nl <- as.list(1:nvars)
        names(nl) <- names
        cols <- logical(nvars)
        if (class(substitute(select)) == 'call') {
            cols[eval(substitute(select), nl, parent.frame())] <- TRUE
            select.vars <- sapply(substitute(select)[-1],as.character)
        } else { # As suggested by Diogo Ferrari (https://dioferrari.com/)
            select.vars <- select
            cols[which(names(nl) %in% select.vars )] <- TRUE
        }

    }
    if(nobs < cs) cs <- nobs
    m <- nobs %/% cs
    r <- nobs %% cs

    seekData(x)
    if(missing(subset)){
        res <- readChunk(x,nrows=nobs,cols=cols)
        names(res) <- names(x)[cols]
        res <- res[select.vars]
        attr(res,"row.names") <- 1:nobs
    }
    else {
        select.cols <- cols
        e <- substitute(subset)
        subset.vars <- all.vars(e)
        subset.cols <- names(x) %in% subset.vars
        cols <- select.cols | subset.cols
        chunk.names <- names(x)[cols]
        chunk.cols <- chunk.names %in% select.vars
        res <- x@.Data[select.cols]
        res.nobs <- 0
        for(i in 1:m){
            chunk <- readChunk(x,nrows=cs,cols=cols)
            names(chunk) <- chunk.names
            use.obs <- eval(e,chunk,parent.frame())
            if(!is.logical(use.obs)) stop("non-logical subset arg")
            if(any(use.obs)){
                res.nobs <- res.nobs + sum(use.obs)
                chunk <- chunk[chunk.cols]
                chunk <- lapply(chunk,"[",use.obs)
                res <- mapply(c_Data,res,chunk,SIMPLIFY=FALSE)
            }
        }
        if(r > 0){
            chunk <- readChunk(x,nrows=r,cols=cols)
            names(chunk) <- chunk.names
            use.obs <- eval(e,chunk,parent.frame())
            if(!is.logical(use.obs)) stop("non-logical subset arg")
            if(any(use.obs)){
                res.nobs <- res.nobs + sum(use.obs)
                chunk <- chunk[chunk.cols]
                chunk <- lapply(chunk,"[",use.obs)
                res <- mapply(c_Data,res,chunk,SIMPLIFY=FALSE)
            }
        }
        for(j in 1:length(res)){
            attributes(res[[j]]) <- attributes(chunk[[j]])
        }
        names(res) <- names(x)[select.cols]
        res <- res[select.vars]
        attr(res,"row.names") <- 1:res.nobs
    }
    new.names <- names(select.vars)
    if(length(new.names) && any(nzchar(new.names))){
        names.res <- names(res)
        names.res[nzchar(new.names)] <- new.names[nzchar(new.names)]
        names(res) <- names.res
    }
    class(res) <- "data.frame"
    new("data.set",res)
})


setMethod("$",signature(x="importer"),
function(x,name){
  x[name]              
})

setMethod("[[",signature(x="importer"),
function(x,i,...){
  x[i]         
})


setMethod("description","importer",function(x){
    res <- lapply(structure(x@.Data,
                            names=x@names),
                  description)
    structure(res,class="descriptions")
})

setMethod("codebook","importer",function(x){
  cs <- getOption("codebook.chunk.size")
  nobs <- nrow(x)
  if(nobs < cs) cs <- nobs
  m <- nobs %/% cs
  r <- nobs %% cs
  nvar <- ncol(x)
  res <- lapply(structure(x@.Data,
                          names=x@names),
                initcodebookEntry)
  seekData(x)
  for(i in 1:m)
    res <- mapply(updatecodebookEntry,res,readData(x,n=cs))
  if(r > 0)
    res <- mapply(updatecodebookEntry,res,readData(x,n=r))
  res <- lapply(res,fixupcodebookEntry)
  new("codebook",res)
})

initcodebookEntry <- function(x){
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
  stats <- switch(spec["Measurement:"],
                  nominal=,ordinal=initcodebookStatsCateg(x),
                  interval=,ratio=initcodebookStatsMetric(x),
                  `Date/time`=initcodebookStatsDatetime(x)
  )
  new("codebookEntry",
      spec = spec,
      stats = stats,
      annotation = annotation
  )
}


updatecodebookEntry <- function(cbe,x){
  switch(cbe@spec["Measurement:"],
         nominal=,ordinal=updatecodebookStatsCateg(cbe,x),
         interval=,ratio=updatecodebookStatsMetric(cbe,x),
         `Date/time`=updatecodebookStatsDatetime(cbe,x)
  )
}


initcodebookStatsCateg <- function(x) {
    vl <- labels(x)
    ic <- inherits(x,"character")
    if(length(vl) || !ic){
        list(tab=list())
    } else {
        list()
    }
    
} 

cbTable1 <- function(x){
    vl <- labels(x)
    vvl <- vl@values
    lvl <- vl@.Data
    valid <- !is.missing2(vvl,x@value.filter)
    i <- match(x@.Data,vvl,nomatch=0L)
    tab <- tabulate(i,nbins=length(lvl))
    names(tab) <- as.character(lvl)
    is.m <- is.missing(x)
    isNA <- is.na(x)
    vl <- labels(x)
    if(length(vl)){
        vvl <- vl@values
        lvl <- vl@.Data
        valid <- !is.missing2(vvl,x@value.filter)
        i <- match(x@.Data,vvl,nomatch=0L)
        tab <- tabulate(i,nbins=length(vvl))
        names(tab) <- as.character(vl@values)
        lab <- sQuote(vl@.Data) 
    }
    else {
        valid <- logical(0)
        tab <- c()
        lab <- c()
        i <- logical(length(x))
    }

    ovld <- sum(!is.m & !i)
    omiss <- sum(is.m & !i & !isNA)
    NAs <- sum(isNA)

    list(tab=tab,
         ovld=ovld,
         omiss=omiss,
         NAs=NAs,
         value.labels=vl,
         value.filter=x@value.filter)
}

update_cbTable1 <- function(current,update){
    if(length(current)){ 
        if(length(current$tab) > length(update$tab)) {
            ii <- match(names(update$tab),names(current$tab))
            current$tab[ii] <- current$tab[ii] + update$tab
        }
        else if(length(current$tab) < length(update$tab)){
            ii <- match(names(current$tab),names(update$tab))
            update$tab[ii] <- current$tab + update$tab[ii]
            current$tab <- update$tab
        }
        else
            current$tab <- current$tab + update$tab
        current$ovld <- current$ovld + update$ovld
        current$omiss <- current$nomiss + update$omiss
        current$NAs <- current$NAs + update$NAs
        return(current)
    }
    else return(update)
}

updatecodebookStatsCateg <- function(cbe,x){
    vl <- labels(x)
    ic <- inherits(x,"character")
    stats <- cbe@stats

    if(length(vl) || !ic){
        tab <- cbTable1(x)
        stats$tab <- update_cbTable1(stats$tab,tab)
    } else {
        stats1 <- cbChar1(x)
        stats <- update_cbChar1(stats,stats1)
    }
    
    cbe@stats <- stats
    cbe
}

cbChar1 <- function(x){
    isna <- is.na(x)
    NAs <- sum(isna)
    descr <- structure(range(x),names=c("Min","Max"))
    list(descr=descr,
         NAs = NAs)
}

update_cbChar1 <- function(current,update){
    if(!length(current)) return(update)
    else {
        list(
            descr = range(c(current$descr,update$descr)),
            NAs = current$NAs + update$NAs
        )
    }
}


initcodebookStatsMetric <- function(x){
    stats <- list()
    if(length(x@value.labels))
        stats$tab <- list()
    stats$moments <- numeric(0)
    stats$range <- numeric(0)
    stats$missings <- integer(0)
    stats
}

updatecodebookStatsMetric <- function(cbe,x){
  stats <- cbe@stats
  if(length(x@value.labels)){
      tab <- cbTable1(x)
      stats$tab <- update_cbTable1(stats$tab,tab)
  }
  miss <- is.missing(x)
  NAs <- is.na(x@.Data)
  x <- x@.Data[!miss & !NAs]
  NAs <- sum(NAs)
  miss <- sum(miss,na.rm=TRUE)
  if(length(x)){
    moments <- Moments(x)
    stats$moments <- if(length(stats$moments)) {
                      N1 <- stats$moments[5]
                      N2 <- moments[5]
                      N <- N1 + N2
                      w1 <- N1/N
                      w2 <- N2/N
                      c(
                        w1*stats$moments[-5] + w2*moments[-5],
                        N = N
                        )
                      }
                  else moments
    stats$range <- if(length(stats$range)) range(stats$range,range(x))
                else range(x)
  }
  stats$missings <- if(length(stats$missings)) stats$missings + c(miss,NAs)
                    else c(miss,NAs)
  cbe@stats <- stats
  cbe
}

initcodebookStatsDatetime <- function(x){
    stats <- list()
    stats$moments <- numeric(0)
    stats$range <- numeric(0)
    stats$missings <- integer(0)
    stats
}


updatecodebookStatsDatetime <- function(cbe,x){
  stats <- cbe@stats
  miss <- is.missing(x)
  NAs <- is.na(x@.Data)
  x <- x@.Data[!miss & !NAs]
  NAs <- sum(NAs)
  miss <- sum(miss,na.rm=TRUE)
  if(length(x)){
    stats$range <- if(length(stats$range)) range(stats$range,range(x))
                else range(x)
  }
  stats$missings <- if(length(stats$missings)) stats$missings + c(miss,NAs)
                    else c(miss,NAs)
  cbe@stats <- stats
  cbe
}


fixupcodebookEntry <- function(cbe){
    switch(cbe@spec["Measurement:"],
    nominal=,ordinal=fixupcodebookEntryCateg(cbe),
    interval=,ratio=fixupcodebookEntryMetric(cbe),
    `Date/time`=fixupcodebookEntryDatetime(cbe)
  )
}

fixupCodebookTable <- function(x,drop.unlabelled=FALSE){
    vl <- x$value.labels
    if(length(vl)){
        vvl <- vl@values
        lvl <- vl@.Data
        valid <- !is.missing2(vvl,x$value.filter)
        tab <- rep(0,length=length(vvl))
        names(tab) <- as.character(vvl)
        tab[names(x$tab)] <- x$tab
        lab <- sQuote(vl@.Data) 
        tab.title <- "Values and labels"
    }
    else {
        valid <- logical(0)
        tab <- c()
        lab <- c()
        tab.title <- "Values"
    }
    
    ovld <- x$ovld
    omiss <- x$omiss
    NAs <- x$NAs

    if(length(ovld) && ovld){
        tab <- c(tab," "=ovld)
        lab <- c(lab,"(unlab.val.)")
        valid <- c(valid,TRUE)
    }

    if(length(omiss) && omiss){
        tab <- c(tab," "=omiss)
        lab <- c(lab,"(unlab.mss.)")
        valid <- c(valid,FALSE)
    }
    if(length(NAs) && NAs){
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
    vperc <- rep(NA,length(tab))
    vtab <- tab[valid]
    Nvalid <- sum(vtab)
    if(Nvalid) vperc[valid] <- 100 * vtab/Nvalid
    else vperc[valid] <- 0
    tperc <- 100 * tab/sum(tab)
    tab <- cbind(N=tab,Valid=vperc,Total=tperc)
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

fixupcodebookEntryCateg <- function(cbe){
    tab <- cbe@stats$tab
    descr <- cbe@stats$descr
    if(length(tab))
        cbe@stats$tab <- fixupCodebookTable(tab,drop.unlabelled=FALSE)
    # else if(length(descr))
    cbe
}

fixupcodebookEntryMetric <- function(cbe){
    tab <- cbe@stats$tab
    if(length(tab))
        tab <- fixupCodebookTable(tab,drop.unlabelled=TRUE)

    moments <- unname(cbe@stats$moments)
    m.1 <- moments[1]
    m.2 <- moments[2] - m.1^2
    m.3 <- moments[3] - 3*moments[2]*m.1 + 2*m.1^3
    m.4 <- moments[4] - 4*moments[3]*m.1 + 6*moments[2]*m.1^2 - 3*m.1^4

    Min <- cbe@stats$range[1]
    Max <- cbe@stats$range[2]

    descr <- c(
        Mean=m.1,
        Variance=m.2,
        Skewness=m.3/m.2^(3/2),
        Kurtosis=m.4/m.2^2-3,
        Min=Min,
        Max=Max
    )

    cbe@stats <- list(tab=tab,descr=descr)
    cbe
}

fixupcodebookEntryDatetime <- function(cbe){

  miss <- cbe@stats$missings[1]
  NAs <- cbe@stats$missings[2]
  Min <- cbe@stats$range[1]
  Max <- cbe@stats$range[2]

  origin <- unname(cbe@spec["Origin:"])
    
  descr <- c(
            Min=format(as.POSIXct(Min,origin=origin)),
            Max=format(as.POSIXct(Max,origin=origin))
            )

  cbe@stats <- list(descr=format(descr))
  cbe
}

setMethod("head",signature(x="importer"),
          function(x,n=20,...){
              y <- utils::head.matrix(x,n=n,...)
              rownames(y) <- 1:n
              return(y)
          })
setMethod("tail",signature(x="importer"),
          function(x,n=20,...){
              y <- utils::tail.matrix(x,n=n,...)
              rownames(y) <- seq.int(to=nrow(x),length.out=n)
              return(y)
          })
