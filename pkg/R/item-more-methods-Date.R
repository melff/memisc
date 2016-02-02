setMethod("as.item","Date",function(x,...){

   annotation <-new("annotation",NULL)
   new("Date.item",
       as.numeric(x),
       measurement="date",
       annotation=annotation)
})

setMethod("as.item","Date.item",as_item_item)

as.Date.numeric.item <- function(x)
                          as.Date(as.numeric(x),
                                  origin=structure(0,class="Date"))

as.data.frame.Date.item <- function (x, row.names = NULL, optional = FALSE, ...)
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0L)
            row.names <- character(0L)
        else row.names <- .set_row_names(nrows)
    }

    x <- as.Date.numeric.item(x)

    value <- list(x)
    if (!optional)
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

str.Date.item <- function(object,give.head=TRUE,width=getOption("width"),...){
  if(give.head){
    hdr <- " Date item"
    cat(hdr,"")
  }
  str(as.Date.numeric.item(object),give.head=FALSE,width=width,...)
}


format.Date.item <- function(x,...){
  format(as.Date.numeric.item(x),...)
}
setMethod("format","Date.item",format.Date.item)

print.Date.item <- function(x,
    width=getOption("width"),
    compress=FALSE,
    ...,print.gap=NULL){
    if(length(x)){
      mkdots <- function(n) paste(rep(".",n),collapse="")
      pg <- if(is.null(print.gap) || compress) 1 else print.gap
      l <- length(x)
      if(compress) {
        x <- x[seq_len(min(width,l))]
        x <- format(x,...)
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
              format(x,...),
              quote=FALSE,print.gap=print.gap)
    }
    else
      print(as.Date.numeric.item(x),print.gap=print.gap)
}

setMethod("show","Date.item",function(object){
  cat("\nDate item",
    if(length(description(object))) sQuote(description(object)) else NULL,
    paste("(",
          "length = ",length(object),
          ")",sep=""),
    "\n\n")
  print.Date.item(object,width=getOption("width"),compress=TRUE)
})
setMethod("print","Date.item",print.Date.item)

setMethod("summary","Date.item",function(object,...,maxsum=100,digits=3){
  ism <- is.missing(object)
  xvalid <- as.Date.numeric.item(object[!ism])
  summary(xvalid)
})

setMethod("codebookEntry","Date.item",function(x){
  annotation <- annotation(x)
  filter <- x@value.filter
  spec <- c(
            "Storage mode:"=storage.mode(x),
            "Measurement:"="Date"
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
      descr=format(c(
                     format(summary(x)),
                    `N.miss.` = sum(ism & !isna),
                    NAs = sum(isna)),
                    justify="left")
      ),
    annotation = annotation
  )
})

xtfrm.Date.item <- function(x) x@.Data
setMethod("as.character","Date.item",function(x,...)
   format(as.Date.numeric.item(x))
)

unique.Date.item <- unique.item.vector

