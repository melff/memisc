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

setReplaceMethod("measurement","data.set",function(x,value){
    mslevels <- names(value)
    for(mlv in mslevels){
        mlv <- as.measurement.level(mlv)
        vars <- value[[mlv]]
        for(var in vars){
            measurement(x[[var]]) <- mlv
        }
    }
    invisible(x)
})

set_measurement <- function(x,...){
    mycall <- match.call(expand.dots=FALSE)
    lst <- mycall$...
    mslevels <- names(lst)
    for(mlv in mslevels){
        mlv <- as.measurement.level(mlv)
        vars <- lst[[mlv]]
        if(inherits(vars,"call")){
            vars <- sapply(vars[-1],as.character)
        }
        else
            vars <- as.character(vars)
        for(var in vars){
            measurement(x[[var]]) <- mlv
        }
    }
    return(x)
}
