spss.portable.file <- function(
    file,
    varlab.file=NULL,
    codes.file=NULL,
    missval.file=NULL,
    count.cases=TRUE,
    to.lower=TRUE
    ){
    file <- path.expand(file)
    check.file(file,error=TRUE)
    ptr <- porStream(file)
    
    data.spec <- parseHeaderPorStream(ptr)
    types <- data.spec$types
    variables <- vector(length(types),mode="list")
    variables[types==0] <- list(new("double.item"))
    variables[types>0] <- list(new("character.item"))
    names(variables) <- names(types)
    
    varlabs <- sapply(data.spec$dictionary,"[[",i="label")
    varlabs <- varlabs[nzchar(varlabs)]

    vallabs <- data.spec$value.labels
    vallabs.vars <- lapply(vallabs,"[[",i="vars")
    vallab.tmp <- lapply(seq_along(vallabs),function(i){
                        ans <- list()
                        ans[vallabs.vars[[i]]] <- list(vallabs[[i]]$value.labels)
                        ans
                      })
    vallabs <- unlist(vallab.tmp,recursive=FALSE)
   
    missings <- lapply(data.spec$dictionary,"[[",i="missing")

    if(length(varlab.file) && check.file(varlab.file,error=TRUE)){
      message("using ",varlab.file)
      varlabs <- spss.parse.variable.labels(varlab.file)
      }
    if(length(codes.file) && check.file(codes.file,error=TRUE)){
      message("using ",codes.file)
      vallabs <- spss.parse.value.labels(codes.file)
      }
    if(length(missval.file) && check.file(missval.file,error=TRUE)){
      message("using ",missval.file)
      missings <- spss.parse.missing.values(missval.file)
      }
    
    if(length(varlabs))
      variables[names(varlabs)] <- mapply("description<-",variables[names(varlabs)],varlabs)
    if(length(vallabs))
      variables[names(vallabs)] <- mapply("labels<-",variables[names(vallabs)],vallabs)
    if(length(missings))
      variables[names(missings)] <- mapply("missing.values<-",variables[names(missings)],missings)
    
    if(count.cases){
        ncases <- .Call("countCasesPorStream",ptr,types)
        seekPorStream(ptr,data.spec$start.data)
    } else
        ncases <- NA
    attr(ptr,"ncases") <- ncases
    
    if(to.lower){
      names(variables) <- tolower(names(variables))
    }
    
    document <- data.spec$document
    data.spec$document <- NULL
    new("spss.portable.importer",
      variables,
      ptr=ptr,
      varlab.file=varlab.file,
      codes.file=codes.file,
      missval.file=missval.file,
      document=document,
      data.spec=data.spec
      )
}
setMethod("initialize","spss.portable.importer",function(.Object,
                                                          variables,
                                                          ptr,
                                                          varlab.file=character(),
                                                          codes.file=character(),
                                                          missval.file=character(),
                                                          document=character(),
                                                          data.spec
                                                          ){
     .Object@.Data <- variables
     .Object@ptr <- ptr
     .Object@varlab.file <- as.character(varlab.file)
     .Object@codes.file <- as.character(codes.file)
     .Object@missval.file <- as.character(missval.file)
     .Object@document <- as.character(document)
     .Object@data.spec <- data.spec
     .Object
})

setMethod("getNobs","spss.portable.importer",function(x){
  ncases <- attr(x@ptr,"ncases")
  if(!length(ncases)) {
    seekPorStream(x@ptr,x@data.spec$start.data)
    attr(x@ptr,"ncases") <- ncases <- .Call("countCasesPorStream",x@ptr,x@data.spec$types)
    seekPorStream(x@ptr,x@data.spec$start.data)
  }
  ncases
})

setMethod("seekData","spss.portable.importer",function(x)
  seekPorStream(x@ptr,x@data.spec$start.data)
)

setMethod("readData","spss.portable.importer",
  function(x,n)
    .Call("readDataPorStream",
      x@ptr,
      what=x,
      nlines=n,
      types=x@data.spec$types
))

setMethod("readSubset","spss.portable.importer",
  function(x,rows,cols)
    .Call("readSubsetPorStream",x@ptr,
      what=x,
      j=cols,i=rows,
      types=x@data.spec$types
))

setMethod("show","spss.portable.importer",
  function(object){
    file.name <- attr(object@ptr,"file.name")
    nobs <- nrow(object)
    nvar <- ncol(object)
    varlab.file <- object@varlab.file
    codes.file <- object@codes.file
    missval.file <- object@missval.file
    cat("\nSPSS portable file",sQuote(file.name),"\n\twith ")
    cat(nvar,"variables and ")
    cat(nobs,"observations\n")
    if(length(varlab.file)) cat("\twith variable labels from file",sQuote(varlab.file),"\n")
    if(length(codes.file)) cat("\twith value labels from file",sQuote(codes.file),"\n")
    if(length(missval.file)) cat("\twith missing value definitions from file",sQuote(missval.file),"\n")
})

