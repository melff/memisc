Iconv <- function(x,from="",to="",...) UseMethod("Iconv")

Iconv.importer <- function(x,from="",to="",...){
  x@.Data <- lapply(x@.Data,Iconv.item,from,to,...)
  return(x)
}

Iconv.item <- function(x,from="",to="",...){
  if(length(ann <- annotation(x)))
    annotation(x) <- Iconv.annotation(ann,from=from,to=to,...)
  if(length(vl <- labels(x)))
    labels(x) <- Iconv.value.labels(vl,from=from,to=to,...)
  return(x)
}

Iconv.value.labels <- function(x,from="",to="",...){
  x@.Data <- iconv(x@.Data,from=from,to=to,...)
  return(x)
}

Iconv.annotation <- function(x,from="",to="",...){
  x@.Data <- iconv(x@.Data,from=from,to=to,...)  
  return(x)
}

Iconv.data.set <- function(x,from="",to="",...){
  x@.Data <- lapply(x@.Data,Iconv.item,from,to,...)
  return(x)
}