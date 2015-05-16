
# setMethod("as.POSIXct","numeric.item",function(x,...){
#    as.POSIXct(x@.Data,...)
# })


as.POSIXct.datetime.item <- function(x, tz = "", ...){
   origin <- if(is.null(x@origin)) "1970-01-01" else x@origin
   if(missing(tz)) {
      
      tz <- if(is.character(x@tzone)) x@tzone else "GMT"
   }
   as.POSIXct(x@.Data,
      tz=tz,
      origin=origin)
}

setMethod("as.item","POSIXct",function(x,...){
   new("datetime.item",as.numeric(x),tzone=attr(x,"tzone"),...)
})

setMethod("as.item","datetime.item",as_item_item)





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
  if (missing(tz) && !is.null(tzone <- attr(x, "tzone"))) tz <- tzone
  format(as.POSIXct.datetime.item(x),format=format,tz=tz,usetz=usetz,...)
}
setMethod("format","datetime.item",format.datetime.item)

print.datetime.item <- function(x,
    use.labels=isTRUE(getOption("print.use.value.labels")),
    width=getOption("width"),
    compress=FALSE,
    usetz=TRUE,...,print.gap=NULL){
    if(length(x)){
      mkdots <- function(n) paste(rep(".",n),collapse="")
      pg <- if(is.null(print.gap) || compress) 1 else print.gap
      l <- length(x)
      if(compress) {
        x <- x[seq_len(min(width,l))]
        x <- format(x,usetz=usetz,...)
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
              format(x,usetz=usetz,...),
              quote=FALSE,print.gap=print.gap)
    }
    else
      print(as.POSIXct.datetime.item(x),print.gap=print.gap)
}

setMethod("show","datetime.item",function(object){
  cat("\nDate/time item",
    if(length(description(object))) sQuote(description(object)) else NULL,
    paste("(",
          "length = ",length(object),
          ", time zone = ",object@tzone,
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


setMethod("codebookEntry","datetime.item",function(x){
  annotation <- annotation(x)
  filter <- x@value.filter
  spec <- c(
            "Storage mode:"=storage.mode(x),
            "Measurement:"="Date/time"
            )
  if(length(filter)) spec <- c(spec,
                              switch(class(filter),
                                        missing.values = c("Missing values:" = format(filter)),
                                        valid.values   = c("Valid values:"   = format(filter)),
                                        valid.range    = c("Valid range:"    = format(filter))
                                        ))
  ism <- is.missing(x)
  isna <- is.na(x)
  new("codebookEntry",
    spec = spec,
    stats = list(
      descr=format(summary(x)),
      `N.miss.` = sum(ism & !isna),
      NAs = sum(isna)
      ),
    annotation = annotation
  )
})


xtfrm.datetime.item <- function(x) x@.Data
setMethod("as.character","datetime.item",function(x,...)
   format(as.POSIXct.datetime.item(x))
)

unique.datetime.item <- unique.item.vector
