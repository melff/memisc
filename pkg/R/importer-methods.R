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

setMethod("codebook","importer",function(x, weights=NULL, unweighted=TRUE, ...){
  if(!missing(weights))
    weights <- deparse(substitute(weights))
  if(length(weights)){
      cat("Weights are from ",weights,"\n")
      # warning("Weights for codebooks of 'importer' objects are not yet supported.")
  }
  
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
  w <- NULL
  w.idx <- match(weights,names(x))
  for(i in 1:m){
    chunk <- readData(x,n=cs)
    if(length(weights)){
      w <- as.numeric(chunk[[w.idx]])
      w[is.na(w)] <- 0
    }
    res <- mapply(updatecodebookEntry,res,chunk,
                  MoreArgs=list(weights=w))
  }
  if(r > 0){
    chunk <- readData(x,n=r)
    if(length(weights)){
      w <- as.numeric(chunk[[w.idx]])
      w[is.na(w)] <- 0
    }
    res <- mapply(updatecodebookEntry,res,chunk,
                  MoreArgs=list(weights=w))
  }
  res <- lapply(res,fixupcodebookEntry,unweighted=unweighted)
  new("codebook",res)
})

initcodebookEntry <- function(x,weighted){
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
                  nominal=,ordinal=initcodebookStatsCateg(x,weights),
                  interval=,ratio=initcodebookStatsMetric(x,weights),
                  `Date/time`=initcodebookStatsDatetime(x)
  )
  new("codebookEntry",
      spec = spec,
      stats = stats,
      annotation = annotation
  )
}


updatecodebookEntry <- function(cbe,x,weights=NULL){
  switch(cbe@spec["Measurement:"],
         nominal=,ordinal=updatecodebookStatsCateg(cbe,x,weights),
         interval=,ratio=updatecodebookStatsMetric(cbe,x,weights),
         `Date/time`=updatecodebookStatsDatetime(cbe,x)
  )
}


initcodebookStatsCateg <- function(x,weights=NULL) {
    vl <- labels(x)
    ic <- inherits(x,"character")
    if(length(vl) || !ic){
        if(length(weights))
            list(tab=list(),wtab=list())
        else
            list(tab=list())
    } else {
        list()
    }
    
} 

updatecodebookStatsCateg <- function(cbe,x,weights=NULL){
    vl <- labels(x)
    ic <- inherits(x,"character")
    stats <- cbe@stats

    if(length(vl) || !ic){
        tab <- codebookTable_item(x,drop.empty=FALSE)
        tab.title <- attr(tab,"title")
        if(length(stats$tab)){
          stats$tab <- stats$tab + tab
        }
        else 
            stats$tab <- tab
        attr(stats$tab,"title") <- tab.title
        if(length(weights)){
            wtab <- codebookTable_item(x,weights=weights,drop.empty=FALSE)
            if(length(stats$wtab))
                stats$wtab <- stats$wtab + wtab
            else 
                stats$wtab <- wtab
        }
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


initcodebookStatsMetric <- function(x,weights=NULL){
    if(length(labels(x))){
        if(length(weights))
            stats <- list(tab=list(),wtab=list())
        else
            stats <- list(tab=list())
    }
    else stats <- list()
    stats$descr <- numeric(0)
    stats$range <- numeric(0)
    stats$missings <- integer(0)
    stats
}

cb_descr <- function(x,w=NULL){
  m <- c(n     = length(x),
         sum.x = sum(x),
         sum.x2 = sum(x^2))
  if(length(w)){
    wm <- c(sum(w),
            sum(w*x),
            sum(w*x^2))
    m <- cbind(Unweighted=m,Weighted=wm)
  }
  else m <- as.matrix(m)
  return(m)
}

updatecodebookStatsMetric <- function(cbe,x,weights=NULL){
  stats <- cbe@stats
  if(length(x@value.labels)){
    tab <- codebookTable_item(x,drop.empty=FALSE)
    tab.title <- attr(tab,"title")
    if(length(stats$tab)){
      stats$tab <- stats$tab + tab
    }
    else 
      stats$tab <- tab
    attr(stats$tab,"title") <- tab.title
    if(length(weights)){
      wtab <- codebookTable_item(x,weights=weights,drop.empty=FALSE)
      if(length(stats$wtab))
        stats$wtab <- stats$wtab + wtab
      else 
        stats$wtab <- wtab
    }
  }
  ismiss <- is.missing(x)
  isna <- is.na(x@.Data)
  x <- x@.Data[!ismiss & !isna]
  NAs <- sum(isna)
  miss <- sum(ismiss,na.rm=TRUE)
  if(length(x)){
    if(length(weights))
      w <- weights[!ismiss & !isna]
    else
      w <- NULL
    descr <- cb_descr(x,w=w)
    stats$descr <- if(length(stats$descr)) stats$descr + descr
                     else descr
    
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


updatecodebookStatsDatetime <- function(cbe,x,weights=NULL){
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


fixupcodebookEntry <- function(cbe,unweighted=TRUE){
    switch(cbe@spec["Measurement:"],
    nominal=,ordinal=fixupcodebookEntryCateg(cbe,unweighted=unweighted),
    interval=,ratio=fixupcodebookEntryMetric(cbe,unweighted=unweighted),
    `Date/time`=fixupcodebookEntryDatetime(cbe)
  )
}

prc <- function(x){
  s <- sum(x,na.rm=TRUE)
  100*x/s
}

fixupcodebookEntryCateg <- function(cbe,unweighted=TRUE){
    tab <- cbe@stats$tab
    wtab <- cbe@stats$wtab
    if(length(tab)){
        tab.title <- attr(tab,"title")
        if(length(wtab)){
            if(unweighted)
                tab <- collect(Unweighted=tab,
                               Weighted=wtab)
            else
                tab <- array(wtab,
                             dim=c(dim(tab),1),
                             dimnames=c(dimnames(tab),
                                        list(NULL)))
        } else {
            tab <- array(tab,
                         dim=c(dim(tab),1),
                         dimnames=c(dimnames(tab),
                                    list(NULL)))
        }
        rn <- rownames(tab)
        ii <- (grepl("(unlab.val.)",rn,fixed=TRUE) | grepl("(unlab.mss.)",rn,fixed=TRUE) |
               grepl("NA M ",rn,fixed=TRUE))
        ii <- ii & tab[,1,1] == 0
        tab <- tab[!ii,,,drop=FALSE]
        d <- dim(tab)
        dn <- dimnames(tab)
        tab[,-1,] <- apply(tab[,-1,,drop=FALSE],2:3,prc)
        dim(tab) <- d
        dimnames(tab) <- dn
        attr(tab,"title") <- tab.title
        cbe@stats <- list(tab=tab)
    }
    cbe
}

fixupcodebookEntryMetric <- function(cbe,unweighted=TRUE){
    stats <- cbe@stats
    tab <- stats$tab
    wtab <- stats$wtab
    if(length(tab)){
        tab.title <- attr(tab,"title")
        if(length(wtab)){
            if(unweighted)
                tab <- collect(Unweighted=tab,
                               Weighted=wtab)
            else
                tab <- array(wtab,
                             dim=c(dim(tab),1),
                             dimnames=c(dimnames(tab),
                                        list(NULL)))
        } else {
            tab <- array(tab,
                         dim=c(dim(tab),1),
                         dimnames=c(dimnames(tab),
                                    list(NULL)))
        }
        rn <- rownames(tab)
        ii <- (grepl("(unlab.val.)",rn,fixed=TRUE) | grepl("(unlab.mss.)",rn,fixed=TRUE) |
               grepl("NA M ",rn,fixed=TRUE))
        ii <- ii & tab[,1,1] == 0
        tab <- tab[!ii,,,drop=FALSE]
        d <- dim(tab)
        dn <- dimnames(tab)
        tab[,-1,] <- apply(tab[,-1,,drop=FALSE],2:3,prc)
        dim(tab) <- d
        dimnames(tab) <- dn
        attr(tab,"title") <- tab.title
    }
    descr <- stats$descr
    if(length(descr)){
        # if(length(dim(descr))!=2) browser()
        cn.d <- colnames(descr)
        descr <- t(t(descr[-1,,drop=FALSE])/descr[1,]) # From sums to (weighted) means ...
        descr[2,] <- sqrt(descr[2,] - descr[1,]^2)
        
        rnge <- stats$range
        if(ncol(descr)==2){
            if(unweighted)
                descr <- rbind(cbind(rnge,rnge),descr)
            else{
                descr <- as.matrix(c(rnge,descr[,2]))
                cn.d <- cn.d[2]
            }
        }
        else
            descr <- as.matrix(c(rnge,descr))
        rownames(descr) <- c("Min","Max","Mean","Std.Dev.")
        colnames(descr) <- cn.d
    }
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
  descr[] <- format(descr[])
  descr <- as.matrix(descr)

  cbe@stats <- list(descr=descr)
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
