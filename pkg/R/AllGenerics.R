setGeneric("as.item",function(x,...)standardGeneric("as.item"))

setGeneric("annotation",function(x)standardGeneric("annotation"))
setGeneric("annotation<-",function(x,value)standardGeneric("annotation<-"))

# setGeneric("description",function(x)standardGeneric("description"))
# setGeneric("description<-",function(x,value)standardGeneric("description<-"))

setGeneric("labels",function(object,...)standardGeneric("labels"))
setGeneric("labels<-",function(x,value)standardGeneric("labels<-"))

setGeneric("has.value.labels",function(x)standardGeneric("has.value.labels"))

setGeneric("is.labelled",function(x)standardGeneric("is.labelled"))

setGeneric("value.filter",function(x)standardGeneric("value.filter"))
setGeneric("value.filter<-",function(x,value)standardGeneric("value.filter<-"))

setGeneric("valid.values",function(x)standardGeneric("valid.values"))
setGeneric("missing.values",function(x)standardGeneric("missing.values"))
setGeneric("valid.range",function(x)standardGeneric("valid.range"))
setGeneric("valid.values<-",function(x,value)standardGeneric("valid.values<-"))
setGeneric("valid.range<-",function(x,value)standardGeneric("valid.range<-"))
setGeneric("missing.values<-",function(x,value)standardGeneric("missing.values<-"))
setGeneric("is.missing",function(x)standardGeneric("is.missing"))
setGeneric("include.missings",function(x,mark="*")standardGeneric("include.missings"))

setGeneric("measurement",function(x)standardGeneric("measurement"))
setGeneric("measurement<-",function(x,value)
  standardGeneric("measurement<-"))

setGeneric("as.measurement.level",function(x)standardGeneric("as.measurement.level"))


setGeneric("as.data.set",function(x,row.names=NULL,...)standardGeneric("as.data.set"))

setGeneric("codebook",function(x)standardGeneric("codebook"))
setGeneric("wild.codes",function(x)standardGeneric("wild.codes"))

setGeneric("Table",function(x,...) standardGeneric("Table"))
setGeneric("Descriptives",function(x,...) standardGeneric("Descriptives"))
# setGeneric("unique",function(x,incomparables,...)standardGeneric("unique"))

setGeneric("query",function(x,pattern,...) standardGeneric("query"))

as.array <- function(x,...)base::as.array(x)
setGeneric("as.array",
  function(x,...)standardGeneric("as.array")
  )

#contrasts <- function(x,...) stats::contrasts(x,...)
setGeneric("contrasts",
  function(x,...) standardGeneric("contrasts")
)

# "contrasts<-" <- function(x,how.many,value) stats::`contrasts<-`(x,how.many,value)
# setGeneric("contrasts<-",
#   function(x,how.many,value) standardGeneric("contrasts<-")
# )

Write <- function(x,...) UseMethod("Write")

# setGeneric("Write",
#            function(object,...)
#              standardGeneric("Write"))
