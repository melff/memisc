
# setMethod("as.POSIXct","numeric.item",function(x,...){
#    as.POSIXct(x@.Data,...)
# })


as.POSIXct.datetime.item <- function(x, tz = "", ...){
   origin <- if(is.null(x@origin)) "1970-01-01" else x@origin
   if(missing(tz)) {
      
      tz <- if(is.character(x@tzone) && !nzchar(x@tzone)) x@tzone else "UTC"
   }
   as.POSIXct(x@.Data,
      tz=tz,
      origin=origin)
}

setMethod("as.item","POSIXct",function(x,...){

   annotation <-new("annotation",NULL)
   new("datetime.item",
       as.numeric(x),
       tzone=attr(x,"tzone"),
       origin=NULL,
       value.labels=NULL,
       value.filter=NULL,
       measurement="date/time",
       annotation=annotation)
})

setMethod("as.item","datetime.item",as_item_item)


setMethod("[",signature(x="datetime.item",i="numeric",j="missing",drop="missing"),function(x,i){
    new("datetime.item",x@.Data[i],
        origin=x@origin,
        tzone=x@tzone,
        value.labels=labels(x),
        value.filter=value.filter(x),
        measurement=measurement(x),
        annotation=annotation(x)
        )
})

setMethod("[",signature(x="datetime.item",i="logical",j="missing",drop="missing"),function(x,i){
    new("datetime.item",x@.Data[i],
        origin=x@origin,
        tzone=x@tzone,
        value.labels=labels(x),
        value.filter=value.filter(x),
        measurement=measurement(x),
        annotation=annotation(x)
        )
})



as.data.frame.datetime.item <- function (x, row.names = NULL, optional = FALSE, ...)
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0L)
            row.names <- character(0L)
        else row.names <- .set_row_names(nrows)
    }

    x <- as.POSIXct.datetime.item(x)

    value <- list(x)
    if (!optional)
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

str.datetime.item <- function(object,give.head=TRUE,width=getOption("width"),...){
  if(give.head){
    hdr <- " Date-time item"
    cat(hdr,"")
  }
  str(as.POSIXct.datetime.item(object),give.head=FALSE,width=width,...)
}




format.datetime.item <- function(x,justify="right",format="",tz="",usetz=FALSE,...){
  if (missing(tz) && !is.null(tzone <- x@tzone)) tz <- tzone
  if(!nzchar(tz)) tz <- "UTC"
  format(as.POSIXct.datetime.item(x),format=format,tz=tz,usetz=usetz,...)
}
#setMethod("format","datetime.item",format.datetime.item)

print.datetime.item <- function(x,
    width=getOption("width"),
    compress=FALSE,
    usetz=TRUE,...,print.gap=NULL){
    if(length(x)){
      mkdots <- function(n) paste(rep(".",n),collapse="")
      pg <- if(is.null(print.gap) || compress) 1 else print.gap
      l <- length(x)
      if(compress) {
        x <- x[seq_len(min(width,l))]
        x <- format.datetime.item(x,usetz=usetz,...)
        x <- trimws(x)
        xw <- cumsum(nchar(x,"w")+1)
        hdr <- paste(" [","1:",length(x),"]",sep="")
        width <- width - nchar(hdr)
        if(any(xw > width)){
          dots <- mkdots(3)
          width <- width - nchar(dots,"w") - 1
          upto <- max(which(xw <= width))
          x <- trimws(format(c(x[seq_len(upto)],dots)))
        }
        cat(hdr,x)
        cat("\n")
      }
      else
        print.default(
              format.datetime.item(x,usetz=usetz,...),
              quote=FALSE,print.gap=print.gap)
    }
    else
      print(as.POSIXct.datetime.item(x),print.gap=print.gap)
}

setMethod("show","datetime.item",function(object){
  tzone <- if(length(object@tzone)) object@tzone else "UTC"
  cat("\nDate/time item",
    if(length(description(object))) sQuote(description(object)) else NULL,
    paste("(",
          "length = ",length(object),
          ", time zone = ",tzone,
          ")",sep=""),
    "\n\n")
  print.datetime.item(object,width=getOption("width"),compress=TRUE,usetz=FALSE)
})
setMethod("print","datetime.item",print.datetime.item)


setMethod("summary","datetime.item",function(object,...,maxsum=100,digits=3){
  ism <- is.missing(object)
  xvalid <- as.POSIXct.datetime.item(object[!ism])
  summary(xvalid)
})

missNAtab <- function(ism,isna,weights=NULL){
  if(!length(weights))
      weights <- rep(1,length(isna))
  isvalid <- !isna & !ism
  isomiss <- ism & !isna
  counts <- c(
      Valid = sum(weights*isvalid),
      "NA" = sum(weights*isna),
      "Other missing" = sum(weights*isomiss),
      Total = sum(weights)
  )
  perc <- 100*counts/counts[length(counts)]
  perc[length(counts)] <- NA
  cbind(N=counts,
        Percent=perc)
}


setMethod("codebookEntry","datetime.item",function(x,weights,...){
  annotation <- annotation(x)
  filter <- x@value.filter
  spec <- c(
            "Storage mode:"=storage.mode(x),
            "Measurement:"="Date/time",
            "Origin:"=x@origin
            )
  if(length(filter)) spec <- c(spec,
                              switch(class(filter),
                                        missing.values = c("Missing values:" = format(filter)),
                                        valid.values   = c("Valid values:"   = format(filter)),
                                        valid.range    = c("Valid range:"    = format(filter))
                                        ))
  ism <- is.missing(x)
  isna <- is.na(x)

  if(any(ism | isna)){
      tab <- missNAtab(ism,isna)
    if(length(weights)){
      wtab <- missNAtab(ism,isna,weights)
      tab <- collect(unweighted=tab,
                     weighted=tab)
    }
    else
      tab <- array(tab,
                   dim=c(dim(tab),1),
                   dimnames=c(dimnames(tab),
                              list(NULL)))
    attr(tab,"title") <- "Valid and missing values"
  } else
    tab <- integer(0)

  x <- as.POSIXct(x)
  descr <- Descriptives(x)
  # if(length(weights)){
  #     wdescr <- Descriptives(x,weights)
  #     descr <- collect(unweighted=format(descr),
  #                      weighted=format(wdescr))
  # }
  # else 
  descr <- as.matrix(format(descr))

  stats <- list(tab=tab,
                descr=descr)
  new("codebookEntry",
    spec = spec,
    stats = stats,
    annotation = annotation
  )
})

## Methods for auxiliar functions

xtfrm.datetime.item <- function(x) x@.Data
mtfrm.datetime.item <- function(x) x@.Data

setMethod("as.character","datetime.item",function(x,...)
   format(as.POSIXct.datetime.item(x))
)

unique.datetime.item <- unique.item.vector
