## TODO: Handle NA in labels part

## labels ################################################################################

setMethod("initialize","value.labels",function(.Object,...){
  args <- list(...)
  values <- args$values
  labels <- args[[1]]
  ii <- order(values)
  values <- values[ii]
  labels <- labels[ii]
  .Object@.Data <- labels
  .Object@values <- values
  .Object
})

setValidity("value.labels",function(object){
  if(length(object@.Data)==length(object@values)) TRUE
  else paste("There are",length(object@values),"values, but",
        length(object@.Data),"labels")
})

setMethod("labels","item",function(object,...)object@value.labels)
setMethod("labels<-",signature(x="item",value="ANY"),function(x,value){
  x@value.labels<-as(value,"value.labels")
  x
})

setMethod("labels<-",signature(x="item",value="NULL"),function(x,value){
  x@value.labels<-NULL
  x
})

setMethod("labels<-",signature(x="vector",value="ANY"),function(x,value)as.item(x,
  labels=as(value,"value.labels")))

format.value.labels <- function(x,...){
  paste(format(x@values,justify="right"),format(sQuote(x@.Data),justify="left"))
}
print.value.labels <- function(x,...) writeLines(format.value.labels(x))
setMethod("show","value.labels",function(object){
  writeLines(c("",
               " Values and labels:",
               "",
                paste("  ",format.value.labels(object)),
                ""))
})

setMethod("[",signature(x="value.labels",i="numeric",j="missing",drop="missing"),
  function(x,i)  new("value.labels",x@.Data[i], values=x@values[i])
)

setMethod("[",signature(x="value.labels",i="logical",j="missing",drop="missing"),
  function(x,i)
  new("value.labels",x@.Data[i], values=x@values[i]))

setAs(from="numeric",to="value.labels",function(from,to){
  if(length(names(from))){
    new("value.labels",names(from),values=unname(from))
  }
  #else new("value.labels",as.character(from),values=from)
  else new("value.labels",character(0),values=logical(0))
})

setAs(from="character",to="value.labels",function(from,to){
  if(length(names(from))){
    new("value.labels",names(from),values=unname(from))
  }
  #else new("value.labels",from,values=from)
  else new("value.labels",character(0),values=logical(0))
})


setAs(from="value.labels",to="numeric",function(from,to)
  structure(as.vector(from@values),names=from@.Data)
)

setAs(from="value.labels",to="character",function(from,to)
  from@.Data
)


setMethod("as.vector","value.labels",function(x,mode="any")
  structure(
    as.vector(x@values,mode=mode),
    names=x@.Data
    )
)


setMethod("Arith",signature(e1="value.labels",e2="ANY"),
  function(e1,e2){
    if(!is(e2,"value.labels"))
      e2 <- as(e2,"value.labels")
    vals <- e1@values
    labs <- e1@.Data
    newvals <- e2@values
    newlabs <- e2@.Data
    upd.vals <- intersect(vals,newvals)
    upd.labs <- newlabs[newvals %in% upd.vals]
    tmp <- setdiff(newvals,upd.vals)
    newlabs <- newlabs[match(tmp,newvals,nomatch=0L)]
    newvals <- tmp
    if(.Generic == "+"){
      labs <- c(labs[vals %nin% upd.vals],upd.labs,newlabs)
      vals <- c(vals[vals %nin% upd.vals],upd.vals,newvals)
      ii <- order(vals)
      vals <- vals[ii]
      labs <- labs[ii]
    }
    else if(.Generic == "-"){
      labs <- labs[vals %nin% upd.vals]
      vals <- vals[vals %nin% upd.vals]
      ii <- order(vals)
      vals <- vals[ii]
      labs <- labs[ii]
    }
    else stop("unsupported operator ",dQuote(.Generic))
    new("value.labels",labs,
      values = vals)
})



setMethod("has.value.labels","ANY",function(x)FALSE)
setMethod("has.value.labels","item",function(x)length(x@value.labels)>0)

setMethod("is.labelled",signature(x="item.vector"),function(x){
  if(!length(x@value.labels)) return(logical(length(x)))
  cl <- x@value.labels
  x %in% cl@values
})

