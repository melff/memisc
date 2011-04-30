Stata.file <- function(file){
    file <- path.expand(file)
    check.file(file,error=TRUE)
    dta <- new.dta(file)
    data.spec <- get.dictionary.dta(dta)
    types <- data.spec$types
    variables <- .Call("dta_make_prototype",types)
    #variables <- lapply(variables,as.item)
    names(variables) <- data.spec$names
    varlabs <- data.spec$varlabs
    varlabs <- varlabs[nchar(varlabs)>0]
    vallabs <- data.spec$value.labels
    missings <- data.spec$missing.values
    
    if(length(varlabs))
      variables[names(varlabs)] <- mapply("description<-",variables[names(varlabs)],varlabs)
    if(length(vallabs))
      variables[names(vallabs)] <- mapply("labels<-",variables[names(vallabs)],vallabs)
    if(length(missings))
      variables[names(missings)] <- mapply("missing.values<-",variables[names(missings)],missings)

    new("Stata.importer",
      variables,
      ptr=dta,
      data.spec=data.spec
    )
}
setMethod("initialize","Stata.importer",function(.Object,
                                                          variables,
                                                          ptr,
                                                          data.spec
                                                          ){
     .Object@.Data <- variables
     .Object@ptr <- ptr
     .Object@data.spec <- data.spec
     .Object
})

setMethod("getNobs","Stata.importer",function(x) x@data.spec$nobs)

setMethod("seekData","Stata.importer",function(x)
  .Call("dta_seek_data",x@ptr)
)

setMethod("readData","Stata.importer",
  function(x,n)
    .Call("dta_read_data",
      x@ptr,
      what=x,
      ncases=n,
      types=x@data.spec$types
))

setMethod("readSubset","Stata.importer",
  function(x,rows,cols)
    .Call("dta_read_subset",x@ptr,
      what=x,
      j=cols,i=rows,
      types=x@data.spec$types
))

setMethod("show","Stata.importer",
  function(object){
    file.name <- attr(object@ptr,"file.name")
    nobs <- nrow(object)
    nvar <- ncol(object)
    cat("\nStata file",sQuote(file.name),"\n\twith ")
    cat(nvar,"variables and ")
    cat(nobs,"observations\n")
})
