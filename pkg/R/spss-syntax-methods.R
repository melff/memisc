spss.fixed.file <- function(
    file,
    columns.file,
    varlab.file=NULL,
    codes.file=NULL,
    missval.file=NULL,
    count.cases=TRUE
    ){
    file <- force(file)
    columns.file <- force(columns.file)
    file <- path.expand(file)
    columns.file <- path.expand(columns.file)
    check.file(file,error=TRUE)
    fptr <- rofile(file)
    check.file(columns.file,error=TRUE)
    data.spec <- spss.parse.data.spec(columns.file)
    types <- data.spec$types
    varlabs <- if(length(varlab.file) && check.file(varlab.file,error=TRUE)) spss.parse.variable.labels(varlab.file)
               else vector(length(types),mode="list")
    vallabs <- if(length(codes.file) && check.file(codes.file,error=TRUE)) spss.parse.labels(codes.file)
               else vector(length(types),mode="list")
    missings <- if(length(missval.file) && check.file(missval.file,error=TRUE)) spss.parse.missing.values(missval.file)
               else vector(length(types),mode="list")
    variables <- vector(length(types),mode="list")
    names(variables) <- names(types)
    variables[types==1] <- list(new("double.item"))
    variables[types==2] <- list(new("character.item"))
    variables[names(varlabs)] <- mapply("description<-",variables[names(varlabs)],varlabs)
    variables[names(vallabs)] <- mapply("labels<-",variables[names(vallabs)],vallabs)
    variables[names(missings)] <- mapply("missing.values<-",variables[names(missings)],missings)
#     for(n in names(variables)){
#       v <- if(types[n]==1) new("double.item") else new("character.item")
#       desc <- varlabs[n]
#       if(is.finite(desc))
#         annotation(v) <- c(description=desc)
#       labs <- vallabs[[n]]
#       if(length(labs))
#         labels(v) <- labs
#       mval <- missings[[n]]
#       if(length(mval))
#         missing.values(v) <- mval
#     }

    nlines <- if(count.cases) {
        maxlenline <- data.spec$stop[length(data.spec$stop)]
        rofseek(fptr,pos=0)
        for(i in seq_len(data.spec$skip)) roreadline(fptr)
        nlines <- .Call("countlines",fptr,maxlenline)
     } else NA_integer_
     attr(fptr,"nlines") <- nlines
     new("spss.fixed.importer",
      variables,
      ptr=fptr,
      columns.file=columns.file,
      varlab.file=varlab.file,
      codes.file=codes.file,
      missval.file=missval.file,
      data.spec=data.spec
      )
}
setMethod("initialize","spss.fixed.importer",function(.Object,
                                                          variables,
                                                          ptr,
                                                          columns.file=character(),
                                                          varlab.file=character(),
                                                          codes.file=character(),
                                                          missval.file=character(),
                                                          document=character(),
                                                          data.spec
                                                          ){
     .Object@.Data <- variables
     .Object@ptr <- ptr
     .Object@columns.file <- as.character(columns.file)
     .Object@varlab.file <- as.character(varlab.file)
     .Object@codes.file <- as.character(codes.file)
     .Object@missval.file <- as.character(missval.file)
     .Object@document <- as.character(document)
     .Object@data.spec <- data.spec
     .Object
})


setMethod("getNobs","spss.fixed.importer",function(x){
  nlines <- attr(x@ptr,"nlines")
  if(!length(nlines)) {
        maxlenline <- x@data.spec$stop[length(x@data.spec$stop)]
        rofseek(x@ptr,pos=0)
        for(i in seq_len(x@data.spec$skip)) roreadline(x@ptr)
        attr(x@ptr,"nlines") <- nlines <- .Call("countlines",x@ptr,maxlenline)
  }
  nlines
})

setMethod("seekData","spss.fixed.importer",function(x){
  rofseek(x@ptr,pos=0)
  for(i in seq_len(x@data.spec$skip))
         roreadline(x@ptr)
})

setMethod("readData","spss.fixed.importer",
  function(x,n)
    readfixed(x@ptr,
      what=x,
      nlines=n,
      start=x@data.spec$start,
      stop=x@data.spec$stop
))

setMethod("readSubset","spss.fixed.importer",
  function(x,rows,cols)
    readfixedsubset(x@ptr,
    what=x,
    j=cols,i=rows,
    start=x@data.spec$start,
    stop=x@data.spec$stop
))

setMethod("show","spss.fixed.importer",
  function(object){
    file.name <- attr(object@ptr,"file.name")
    nobs <- nrow(object)
    nvar <- ncol(object)
    varlab.file <- object@varlab.file
    codes.file <- object@codes.file
    missval.file <- object@missval.file
    cat("\nSPSS fixed column file",sQuote(file.name),"\n\twith ")
    cat(nvar,"variables and ")
    cat(nobs,"observations\n")
    if(length(varlab.file)) cat("\twith variable labels from file",sQuote(varlab.file),"\n")
    if(length(codes.file)) cat("\twith value labels from file",sQuote(codes.file),"\n")
    if(length(missval.file)) cat("\twith missing value definitions from file",sQuote(missval.file),"\n")
})

