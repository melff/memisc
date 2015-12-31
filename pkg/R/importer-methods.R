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
  res <- readSubset(x,rows,cols)
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


setMethod("subset","importer",
  function (x, subset, select, drop = FALSE, ...){
    ncases <- nrow(x)
    names <- names(x)
    nvars <- length(names)

    if (missing(subset))
        r <- rep(TRUE,ncases)
    else {
        e <- substitute(subset)
        subset.vars <- all.vars(e)
        r.s <- rep(TRUE,ncases)
        j.s <- names %in% subset.vars
        r <- NA
        r <- eval(e, x[r.s,j.s,drop=FALSE], parent.frame())
        if (!is.logical(r))
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    if (missing(select)) x[r,, drop = drop,...]
    else {
        nl <- as.list(1:nvars)
        names(nl) <- names
        use <- logical(nvars)
        use[eval(substitute(select), nl, parent.frame())] <- TRUE
        y <- x[r, use, drop = drop,...]
        vars <- sapply(substitute(select)[-1],as.character)
        ii <- match(vars,names[use])
        y <- y[ii]
        new.names <- names(vars)
        if(length(new.names) && any(nzchar(new.names))){
          names.y <- names(y)
          names.y[nzchar(new.names)] <- new.names[nzchar(new.names)]
          names(y) <- names.y
        }
        y
   }   
})


setMethod("description","importer",function(x){
  res <- lapply(x,description)
  structure(res,class="descriptions")
})

setMethod("codebook","importer",function(x){
  cs <- getOption("codebook.chunk.size")
  nobs <- nrow(x)
  if(nobs < cs) cs <- nobs
  m <- nobs %/% cs
  r <- nobs %% cs
  nvar <- ncol(x)
  res <- lapply(x,initcodebookEntry)
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
  new("codebookEntry",
      spec = spec,
      stats = list(),
      annotation = annotation
  )
}

updatecodebookEntry <- function(cbe,x){
  res <- if(cbe@spec["Storage mode:"] == "character")
    updatecodebookStatsChar(cbe,x)
  else switch(cbe@spec["Measurement:"],
    nominal=,ordinal=updatecodebookStatsCateg(cbe,x),
    interval=,ratio=updatecodebookStatsMetric(cbe,x)
  )
  if(!length(res)) browser()
  res
}


updatecodebookStatsCateg <- function(cbe,x){
  tab <- Table(x,style="codebook")
  stats <- cbe@stats
  stats$tab <- if(length(stats$tab)){ 
                                    if(nrow(stats$tab) > nrow(tab)) {
                                      ii <- match(trimws(rownames(tab)),trimws(rownames(stats$tab)))
                                      stats$tab[ii,] <- stats$tab[ii,] + tab
                                      }
                                    else if(nrow(stats$tab) < nrow(tab)){
                                      ii <- match(trimws(rownames(stats$tab)),trimws(rownames(tab)))
                                      tab[ii,] <- stats$tab + tab[ii,]
                                      tab
                                      }
                                    else
                                      stats$tab + tab
                                    }
             else tab
  cbe@stats <- stats
  cbe
}

updatecodebookStatsMetric <- function(cbe,x){
  stats <- cbe@stats
  if(length(x@value.labels)){
    tab <- Table(x,style="codebook")
    stats$tab <- if(length(stats$tab)){ 
                                    if(nrow(stats$tab) > nrow(tab)) {
                                      ii <- match(trimws(rownames(tab)),trimws(rownames(stats$tab)))
                                      stats$tab[ii,] <- stats$tab[ii,] + tab
                                      }
                                    else if(nrow(stats$tab) < nrow(tab)){
                                      ii <- match(trimws(rownames(stats$tab)),trimws(rownames(tab)))
                                      tab[ii,] <- stats$tab + tab[ii,]
                                      tab
                                      }
                                    else
                                      stats$tab + tab
                                    }
              else tab
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


updatecodebookStatsChar <- function(cbe,x){
  stats <- cbe@stats
  if(length(x@value.labels)){
    tab <- Table(x,style="codebook")
    stats$tab <- if(length(stats$tab)) stats$tab + tab
              else tab
  }
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
  if(cbe@spec["Storage mode:"] == "character")
    fixupcodebookEntryChar(cbe)
  else switch(cbe@spec["Measurement:"],
    nominal=,ordinal=fixupcodebookEntryCateg(cbe),
    interval=,ratio=fixupcodebookEntryMetric(cbe)
  )
}

fixupcodebookEntryCateg <- function(cbe){
  tab <- cbe@stats$tab
  valid <- !is.na(tab[,2])
  Nvalid <- sum(tab[valid,1])
  if(Nvalid)
    tab[valid,2] <- 100*tab[valid,1]/Nvalid
  tab[,3] <- 100*tab[,1]/sum(tab[,1])
  cbe@stats$tab <- tab
  cbe
}

fixupcodebookEntryMetric <- function(cbe){
  tab <- cbe@stats$tab
  if(length(tab)){
    valid <- !is.na(tab[,2])
    Nvalid <- sum(tab[valid,1])
    if(Nvalid)
      tab[valid,2] <- 100*tab[valid,1]/Nvalid
    tab[,3] <- 100*tab[,1]/sum(tab[,1])
    cbe@stats$tab <- tab
  }
  
  moments <- unname(cbe@stats$moments)
  m.1 <- moments[1]
  m.2 <- moments[2] - m.1^2
  m.3 <- moments[3] - 3*moments[2]*m.1 + 2*m.1^3
  m.4 <- moments[4] - 4*moments[3]*m.1 + 6*moments[2]*m.1^2 - 3*m.1^4
  miss <- cbe@stats$missings[1]
  NAs <- cbe@stats$missings[2]
  Min <- cbe@stats$range[1]
  Max <- cbe@stats$range[2]

  descr <- c(
            Mean=m.1,
            Variance=m.2,
            Skewness=m.3/m.2^(3/2),
            Kurtosis=m.4/m.2^2-3,
            Min=Min,
            Max=Max,
            Miss.= if(miss) miss else NULL,
            NAs= if(NAs) NAs else NULL
            )

  cbe@stats <- list(tab=tab,descr=descr)
  cbe
}

fixupcodebookEntryChar <- function(cbe){
  tab <- cbe@stats$tab
  if(length(tab)){
    valid <- !is.na(tab[,2])
    Nvalid <- sum(tab[valid,1])
    if(Nvalid)
      tab[valid,2] <- 100*tab[valid,1]/Nvalid
    tab[,3] <- 100*tab[,1]/sum(tab[,1])
    cbe@stats$tab <- tab
  }
  
  miss <- cbe@stats$missings[1]
  NAs <- cbe@stats$missings[2]
  Min <- cbe@stats$range[1]
  Max <- cbe@stats$range[2]

  descr <- c(
            Min=Min,
            Max=Max,
            Miss.= if(miss) miss else NULL,
            NAs= if(NAs) NAs else NULL
            )

  cbe@stats <- list(tab=tab,descr=descr)
  cbe
}