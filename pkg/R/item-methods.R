as_item_item <- function(x,
  labels=NULL,
  missing.values=NULL,
  valid.values=NULL,
  valid.range=NULL,
  value.filter=NULL,
  measurement=NULL,
  annotation=attr(x,"annotation"),
  ...
  ){
  if(!missing(labels))
    x@value.labels <- if(length(labels)) as(labels,"value.labels") else NULL
  if(!(missing(missing.values) && missing(valid.values) && missing(valid.range) && missing(value.filter))){
    mv <- length(missing.values) > 0
    vv <- length(valid.values) > 0
    vr <- length(valid.range) > 0
    vf <- length(value.filter) > 0
    if(mv + vv + vr + vf > 1)
      stop("to many value filter arguments")
    x@value.filter <- if(mv) as(missing.values,"missing.values")
                    else if(vv) as(valid.values,"valid.values")
                    else if(vr) as(valid.range,"valid.range")
                    else if(vf) {
                          if(!is(value.filter,"value.filter"))
                            stop("'value.filter' not of appropriate class")
                          value.filter
                          }
                  else NULL
  }
  if(!missing(measurement)){
    measurement <- if(length(measurement)) match.arg(measurement,c("nominal","ordinal","interval","ratio"))
                  else if(length(labels) && !length(measurement)) "nominal" else "interval"
    measurement(x) <- measurement
  }
  if(!missing(annotation)){
    annotation <- if(length(annotation))
                  new("annotation",structure(as.character(annotation),names=names(annotation)))
                  else x@annotation
    annotation(x) <- annotation
  }
  x
}

setMethod("as.item","double.item",as_item_item)
setMethod("as.item","integer.item",as_item_item)
setMethod("as.item","character.item",as_item_item)

setMethod("as.item","numeric",function(x,
  labels=NULL,
  missing.values=NULL,
  valid.values=NULL,
  valid.range=NULL,
  value.filter=NULL,
  measurement=NULL,
  annotation=attr(x,"annotation"),
  ...
  ){
  value.labels <- if(length(labels)) as(labels,"value.labels") else NULL
  mv <- length(missing.values) > 0
  vv <- length(valid.values) > 0
  vr <- length(valid.range) > 0
  vf <- length(value.filter) > 0
  if(mv + vv + vr + vf > 1)
    stop("to many value filter arguments")
  value.filter <- if(mv) as(missing.values,"missing.values")
                  else if(vv) as(valid.values,"valid.values")
                  else if(vr) as(valid.range,"valid.range")
                  else if(vf) {
                        if(!is(value.filter,"value.filter"))
                          stop("'value.filter' not of appropriate class")
                        value.filter
                        }
  measurement <- if(length(measurement)) match.arg(measurement,c("nominal","ordinal","interval","ratio"))
                 else if(length(value.labels) && !length(measurement)) "nominal" else "interval"
  annotation <- new("annotation",structure(as.character(annotation),names=names(annotation)))
  cl <- paste(storage.mode(x),"item",sep=".")
  new(cl,
    x,
    value.labels=value.labels,
    value.filter=value.filter,
    measurement=measurement,
    annotation=annotation
  )
})

setMethod("as.item","logical",function(x,...) {
  y <- as.integer(x)
  attr(y,"annotation") <- attr(x,"annotation")
  as.item(y,...)
})
setMethod("as.item","factor",function(x,...){
    y <- as.integer(x)
    attr(y,"annotation") <- attr(x,"annotation")
    y <- as.item(y,
            labels=new("value.labels",levels(x),values=seq_along(levels(x))),
            measurement="nominal",
            ...)
    attr(y,"contrasts") <- attr(x,"contrasts")
    y
})
setMethod("as.item","ordered",function(x,...){
    y <- as.integer(x)
  attr(y,"annotation") <- attr(x,"annotation")
    y <- as.item(y,
                labels=new("value.labels",levels(x),values=seq_along(levels(x))),
                measurement="ordinal",
                ...)
    attr(y,"contrasts") <- attr(x,"contrasts")
    y
})

setMethod("as.item","character",function(x,
  labels=NULL,
  missing.values=NULL,
  valid.values=NULL,
  valid.range=NULL,
  value.filter=NULL,
  measurement=NULL,
  annotation=attr(x,"annotation"),
  ...
  ){
  value.labels <- if(length(labels)) as(labels,"value.labels") else NULL
  mv <- length(missing.values) > 0
  vv <- length(valid.values) > 0
  vr <- length(valid.range) > 0
  vf <- length(value.filter) > 0
  if(mv + vv + vr + vf > 1)
    stop("to many value filter arguments")
  value.filter <- if(mv) as(missing.values,"missing.values")
                  else if(vv) as(valid.values,"valid.values")
                  else if(vr) as(valid.range,"valid.range")
                  else if(vf) {
                        if(!is(value.filter,"value.filter"))
                          stop("'value.filter' not of appropriate class")
                        value.filter
                        }
  measurement <- if(length(measurement)) match.arg(measurement,c("nominal","ordinal"))
                 else "nominal"
  annotation <- new("annotation",structure(as.character(annotation),names=names(annotation)))
  new("character.item",x,
    value.labels=value.labels,
    value.filter=value.filter,
    measurement=measurement,
    annotation=annotation
  )
})

setMethod("[",signature(x="item.vector",i="numeric",j="missing",drop="missing"),function(x,i){
  structure(new(class(x),x@.Data[i],
    value.labels=labels(x),
    value.filter=value.filter(x),
    measurement=measurement(x),
    annotation=annotation(x)
  ),contrasts=attr(x,"contrasts"))
})

setMethod("[",signature(x="item.vector",i="logical",j="missing",drop="missing"),function(x,i){
  structure(new(class(x),x@.Data[i],
    value.labels=labels(x),
    value.filter=value.filter(x),
    measurement=measurement(x),
    annotation=annotation(x)
  ),contrasts=attr(x,"contrasts"))
})


str.item.vector <- function(object,give.head=TRUE,width=getOption("width"),...){
  if(give.head){
    hdr <- switch(measurement(object),
                  nominal=" Nmnl. item",
                  ordinal=" Ordl. item",
                  interval=" Itvl. item",
                  ratio=" Rto. item"
                  )
    vl <- labels(object)
    if(length(vl)){
      vlab <- sQuote(vl@.Data)
      vlval <- vl@values
      vlab <- ifelse(nchar(vlab)>10,paste(substr(vlab,start=1,stop=7),"..'",sep=""),
                    vlab)
      lbl.header <- paste("w/",length(vl@.Data),"labels for")
      hdr <- paste(hdr,lbl.header)
      lbl.list <- paste(vlval)
      lbl.h.w <- nchar(hdr,"w") +1 + cumsum(nchar(lbl.list,"w")+1)
      use <- lbl.h.w < width*.6
      if(any(use)) {
        use <- min(3,max(which(use)))
        if(use < length(lbl.list)){
          lbl.list <- c(lbl.list[1:use],"...")
        }
        lbl.list <- paste(lbl.list,collapse=",")
        hdr <- paste(hdr,lbl.list)
      }
    }
    vf <- object@value.filter
    if(length(vf)){
      filt.hdr <- switch(class(vf),
          missing.values="ms.v.",
          valid.values="vd.v.",
          valid.range="vd.r."
          )
      hdr <- paste(hdr,filt.hdr,sep=" + ")
    }
    cat(hdr,"")
  }
  str(object@.Data,give.head=give.head,width=width,...)
}

str.double.item <- str.item.vector
str.integer.item <- str.item.vector
str.character.item <- str.item.vector

## unique ###############################################################################

unique.item.vector <- function(x, incomparables = FALSE, ...){
  y <- new(class(x),unique(x@.Data))
  attributes(y) <- attributes(x)
  y
}

unique.double.item <- unique.item.vector
unique.integer.item <- unique.item.vector
unique.character.item <- unique.item.vector


## unique ###############################################################################

setMethod("unique","item.vector",function(x, incomparables = FALSE, ...)
  new(class(x),
    .Data=callNextMethod(),
    annotation=x@annotation,
    value.labels=x@value.labels,
    value.filter=x@value.filter,
    measurement=x@measurement
  )
)

## coercion #############################################################################


as.data.frame.character.item <-
as.data.frame.double.item <-
as.data.frame.integer.item <- function (x, row.names = NULL, optional = FALSE, ...)
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0L)
            row.names <- character(0L)
        else row.names <- .set_row_names(nrows)
    }

    x <- if(is.ordinal(x)) as.ordered(x)
    else if(is.nominal(x)) as.factor(x)
    else as.vector(x)

    value <- list(x)
    if (!optional)
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

setMethod("as.vector","item",function(x,mode = "any"){
    ism <- is.missing(x)
    x <- callNextMethod()
    if(mode=="any") mode <- storage.mode(x)
    x[ism] <- as.vector(NA,mode=mode)
    x
})

setMethod("as.numeric","item",function(x,...) as.vector(x,mode="double"))
setMethod("as.integer","item",function(x,...) as.vector(x,mode="integer"))

setMethod("as.ordered","item.vector",function(x){
    labels <- x@value.labels
    if(length(labels)){
      values <- labels@values
      labels <- labels@.Data
    }
    else {
      values <- labels <- sort(unique(x@.Data))
    }
    filter <- x@value.filter
    use.levels <- if(length(filter)) is.valid2(values,filter) else TRUE
    f <- suppressWarnings(ordered(x@.Data,levels=values[use.levels],labels=labels[use.levels]))
    if(length(attr(x,"contrasts")))
      attr(f,"contrasts") <- contrasts(x)
    f
})

setMethod("as.factor","item.vector",function(x){
    labels <- x@value.labels
    if(length(labels)){
      values <- labels@values
      labels <- labels@.Data
    }
    else {
      values <- labels <- sort(unique(x@.Data))
    }
    filter <- x@value.filter
    use.levels <- if(length(filter)) is.valid2(values,filter) else TRUE
    f <- suppressWarnings(factor(x@.Data,levels=values[use.levels],labels=labels[use.levels]))
    if(length(attr(x,"contrasts")))
      contrasts(f) <- contrasts(x)
    f
})

setMethod("as.character","item.vector",function(x,use.labels=TRUE,...){
  if(use.labels && length(vl <- labels(x))){
    i <- match(x,vl@values)
    y <- vl@.Data[i]
    y[is.na(y)] <- as.character(x@.Data[is.na(y)])
    y
  }
  else as.character(x@.Data)
})


## utilities #############################################################################

relabel.item <- function(x,...,gsub=FALSE,fixed=TRUE,warn=TRUE){
  subst <- c(...)
  vl <- x@value.labels
  vll <- vl@.Data
  if(gsub){
    for(i in 1:length(subst)){
      vll <- gsub(names(subst[i]),subst[i],vll,fixed=fixed)
    }
  }
  else {
    i <- match(names(subst),vll)
    if(any(is.na(i))) {
      if(warn) warning("undefined label(s) selected")
      if(any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    vll[i] <- subst
  }
  vll[i] <- subst
  vl@.Data <- vll
  x@value.labels <- vl
  return(x)
}

setMethod("relabel4","item",function(x,...)relabel.item(x,...))


setMethod("summary","item.vector",function(object,...,maxsum=100,digits=3)
  switch(measurement(object),
    nominal = smry.nominal.vector(x=object,...,maxsum=maxsum),
    ordinal = smry.ordinal.vector(x=object,...,maxsum=maxsum),
    interval = smry.interval.vector(x=object,...,maxsum=maxsum,digits=digits),
    ratio = smry.ratio.vector(x=object,...,maxsum=maxsum,digits=digits)
    )
)

smry.nominal.vector <- smry.ordinal.vector <- function(x,...,maxsum=100,digits=3){
  ism <- is.missing(x)
  isna <- is.na(x)
  xvalid <- x@.Data[!ism]
  uvalid <- sort(unique(xvalid))
  tabvalid <- if(has.value.labels(x)) {
              vl <- x@value.labels
              vv <- match(uvalid,vl@values)
              nms <- vl@.Data[vv]
              nms[is.na(nms)] <- ""
              structure(tabulate(match(xvalid,uvalid)),names=nms)
              }
              else structure(tabulate(match(xvalid,uvalid)),names=uvalid)
  tabmis <- if(any(ism)) {
              xmis <- x@.Data[ism & !isna]
              umis <- sort(unique(xmis))
              if(has.value.labels(x)) {
                vl <- x@value.labels
                vv <- match(umis,vl@values)
                nms <- vl@.Data[vv]
                nms[is.na(nms)] <- ""
                structure(tabulate(match(xmis,umis)),names=paste("*",nms,sep=""))
              }
              else structure(tabulate(match(xmis,umis)),names=paste("*",umis,sep=""))
            } else integer(0)
  tabna <- if(any(isna)){
              c("NAs"=sum(isna)) #'
            } else integer(0)
  if(length(tabvalid)+length(tabmis)+length(tabna)>maxsum){
    if(length(tabmis)) tabmis <- c("missing values"=sum(tabmis))
    maxsum <- maxsum - length(tabmis) - length(tabna)
    if(any(seq_along(tabvalid) > maxsum)){
      maxsum <- maxsum - 1
      over <- seq_along(tabvalid) > maxsum
      tabvalid <- c(tabvalid[!over],"(Other)"=sum(tabvalid[over]))
    }
  }
  as.table(c(tabvalid,tabmis,tabna))
}

smry.interval.vector <- smry.ratio.vector <- function(x,...,maxsum=100,digits=3){
  ism <- is.missing(x)
  isna <- is.na(x)
  xvalid <- x@.Data[!ism]
  tabvalid <- summary.default(xvalid)
  tabmis <- if(any(ism)) {
              xmis <- x@.Data[ism & !isna]
              c("Missings"=length(xmis))
            } else integer(0)
  tabna <- if(any(isna)){
              c("NAs"=sum(isna))
            } else integer(0)
#   if(length(tabvalid)+length(tabmis)+length(tabna)>maxsum){
#     if(length(tabmis)) tabmis <- c("missing values"=sum(tabmis))
#     maxsum <- maxsum - length(tabmis) - length(tabna)
#     if(any(seq_along(tabvalid) > maxsum)){
#       over <- seq_along(tabvalid) > maxsum
#       tabvalid <- tabvalid[!over]
#     }
#   }
  as.table(c(tabvalid,tabmis,tabna))
}

format.item.vector <- function(x,use.labels=getOption("print.use.value.labels"),justify="right",...){
  ism <- is.missing(x) & !is.na(x)
  if(use.labels && has.value.labels(x))
    x <- as.character(x,use.labels=use.labels)
  x <- format.default(x,trim=TRUE,justify="none",...)
  x[ism] <- paste("*",x[ism],sep="")
#   browser()
  format(x,justify=justify,...)
}
setMethod("format","item.vector",format.item.vector)

print.item.vector <- function(x,
    use.labels=isTRUE(getOption("print.use.value.labels")),
    width=getOption("width"),
    compress=FALSE,...,print.gap=NULL){
    if(length(x)){
      mkdots <- function(n) paste(rep(".",n),collapse="")
      pg <- if(is.null(print.gap) || compress) 1 else print.gap
      l <- length(x)
      if(compress) {
        x <- x[seq_len(min(width,l))]
        x <- format(x,use.labels=use.labels,...)
        x <- trimws(x)
        xw <- cumsum(nchar(x,"w")+1)
        hdr <- paste(" [","1:",l,"]",sep="")
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
              format(x,use.labels=use.labels,...),
              quote=FALSE,print.gap=print.gap)
    }
    else
      print(as.vector(x),print.gap=print.gap)
}

setMethod("show","item.vector",function(object){
  cat("\nItem",
    if(length(description(object))) sQuote(description(object)) else NULL,
    paste("(",
    "measurement: ",measurement(object),", ",
    "type: ",storage.mode(object),", ",
    "length = ",length(object),")",sep=""),
    "\n\n")
  print.item.vector(object,width=getOption("width"),compress=TRUE)
})
setMethod("print","item.vector",print.item.vector)

setMethod("Compare",signature(e1="numeric.item",e2="character"),
  function(e1,e2){
    if(length(e1@value.labels))
      switch(.Generic,
        "=="=,
        "!="=Compare_lvc1(.Generic,e1,e2),
        callNextMethod()
        )
     else callNextMethod()
})
setMethod("Compare",signature(e1="character",e2="numeric.item"),
  function(e1,e2){
    if(length(e1@value.labels))
      switch(.Generic,
        "=="=,
        "!="=Compare_lvc1(.Generic,e2,e1),
        callNextMethod()
        )
     else callNextMethod()
})
setMethod("Arith",signature(e1="numeric",e2="numeric.item"),
  function(e1,e2){
    e1 <- as.vector(e1)
    e2 <- as.vector(e2)
    callNextMethod()
})
setMethod("Arith",signature(e1="numeric.item",e2="numeric"),
  function(e1,e2){
    e1 <- as.vector(e1)
    e2 <- as.vector(e2)
    callNextMethod()
})
setMethod("Math","numeric.item",function(x){
  x <- as.vector(x)
  callNextMethod()
})
setMethod("Math2","numeric.item",function(x,digits){
  x <- as.vector(x)
  callNextMethod()
})
setMethod("Summary","numeric.item",function(x,...,na.rm=FALSE){
  x <- x[!is.missing(x)]
  callNextMethod()
})


Compare_lvc <- function(.Generic,e1,e2){
  if(is(e1,"character")) Compare_lvc1(.Generic,e2,e1)
  else Compare_lvc1(.Generic,e1,e2)
}

Compare_lvc1 <- function(.Generic,e1,e2){
  vl <- labels(e1)
  e1 <- vl@.Data[match(e1,vl@values)]
  ans <- callGeneric(e1,e2)
}

setMethod("%in%",signature(x="numeric.item",table="character"),function(x,table){
   vl <- labels(x)
   x <- vl@.Data[match(x,vl@values)]
   x %in% table
})

## Methods for the auxiliary helper function for 'sort' and 'order'

xtfrm.integer.item <- function(x) x@.Data
xtfrm.numeric.item <- function(x) x@.Data
xtfrm.double.item <- function(x) x@.Data
xtfrm.character.item <- function(x) as.integer(as.factor(x@.Data))

## rep

setMethod("rep","item.vector",function(x,...){

  x@.Data <- rep(x@.Data,...)
  x
})



