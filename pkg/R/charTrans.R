charTrans <- function(x, old = "", new = "", ...) UseMethod("charTrans")

charTrans.character <- function(x, old = "", new = "", ...) 
                          chartr(old = old, new = new, x = x)

charTrans.importer <- function(x, old = "", new = "", ...){
  x@.Data <- lapply(x@.Data, charTrans.item, old = old, new = new, ...)
  return(x)
}

charTrans.item <- function(x, old = "", new = "", ...){
  if(length(ann <- annotation(x)))
    annotation(x) <- charTrans.annotation(ann, old = old, new = new, ...)
  if(length(vl <- labels(x)))
    labels(x) <- charTrans.value.labels(vl, old = old, new = new, ...)
  return(x)
}

charTrans.value.labels <- function(x, old = "", new = "", ...){
  x@.Data <- chartr(x = x@.Data, old = old, new = new, ...)
  return(x)
}

charTrans.annotation <- function(x,old="",new="",...){
  x@.Data <- chartr(x = x@.Data, old = old, new = new, ...)
  return(x)
}

charTrans.data.set <- function(x,old="",new="",...){
  x@.Data <- lapply(x@.Data, charTrans.item, old = old, new = new, ...)
  return(x)
}