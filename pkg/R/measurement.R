## levels of measurement ################################################################

setMethod("measurement","ANY",function(x)NULL)
setMethod("measurement","item",function(x){
  if(length(x@measurement)) return(x@measurement)
  else if(length(x@value.labels) || is.character(x)) return("nominal")
  else return("interval")
})

setReplaceMethod("measurement","item",function(x,value){
  if(length(value)) value <- as.measurement.level(value)
  x@measurement <- value
  invisible(x)
})

setMethod("as.measurement.level","character",function(x=c("interval","nominal","ordinal","ratio")) match.arg(x))
setMethod("as.measurement.level","NULL",function(x) "interval")

is.ordinal <- function(x) measurement(x) == "ordinal"
is.nominal <- function(x) measurement(x) == "nominal"
is.interval <- function(x) measurement(x) == "interval"
is.ratio <- function(x) measurement(x) == "ratio"

