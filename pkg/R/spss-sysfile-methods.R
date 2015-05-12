spss.system.file <- function(
    file,
    varlab.file=NULL,
    codes.file=NULL,
    missval.file=NULL,
    count.cases=TRUE,
    to.lower=TRUE
    ){
    file <- path.expand(file)
    check.file(file,error=TRUE)
    ptr <- .Call("NewSysFile",file)
    
    data.spec <- parseSysHeader(ptr)
    types <- data.spec$types

    varnames <- names(data.spec$variables)
    
    varlabs <- lapply(data.spec$variables,"[[",i="label")
    varlabs <- unlist(varlabs[sapply(varlabs,length)>0])  
    vallabs <- data.spec$value.labels
    vallabs.vars <- lapply(vallabs,"[[",i="variables")
    vallabs.vars <- lapply(vallabs.vars,function(i)names(types)[i])
    vallab.tmp <- lapply(seq_along(vallabs),function(i){
                        ans <- list()
                        ans[vallabs.vars[[i]]] <- list(vallabs[[i]]$labels)
                        ans
                      })
    vallabs <- unlist(vallab.tmp,recursive=FALSE)
    
    missings <- mapply(function(v,r)list(values=v,range=r),
                          data.spec$missings$values,
                          data.spec$missings$ranges,
                          SIMPLIFY=FALSE
                          )
    

                          
    missings <- lapply(missings,function(x)if(!length(x$values) & !length(x$range))NULL else x)
    missings <- missings[sapply(missings,length)>0]
    
    variables <- vector(length(types),mode="list")
    names(variables) <- names(types)
    variables[types==0] <- list(new("double.item"))
    variables[types>0] <- list(new("character.item"))
    variables[types<0] <- NULL

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

    ncases <- data.spec$header$ncases
    if(ncases < 0){
      if(count.cases){
          .Call("rewind_sysfile",ptr)
          ncases <- .Call("count_cases_sysfile",ptr)
          .Call("rewind_sysfile",ptr)
      }
      else
          ncases <- NA
    }
    attr(ptr,"ncases") <- ncases
    document <- data.spec$document
    data.spec$document <- NULL
    
    if(to.lower){
      names(variables) <- tolower(names(variables))
    }
    
    
    new("spss.system.importer",
      variables,
      ptr=ptr,
      varlab.file=varlab.file,
      codes.file=codes.file,
      missval.file=missval.file,
      document=document,
      data.spec=data.spec
      )
}
setMethod("initialize","spss.system.importer",function(.Object,
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


setMethod("getNobs","spss.system.importer",function(x){
  ncases <- attr(x@ptr,"ncases")
  if(!length(ncases)) {
    .Call("rewind_sysfile",x@ptr)
    attr(x@ptr,"ncases") <- ncases <- .Call("count_cases_sysfile",x@ptr)
    .Call("rewind_sysfile",x@ptr)
  }
  ncases
})

setMethod("seekData","spss.system.importer",function(x){
  if(!.Call("check_pointer",x@ptr)) stop("pointer is NULL, you need to recreate the object")
  .Call("rewind_sysfile",x@ptr)
})

setMethod("readData","spss.system.importer",
  function(x,n){
    if(!.Call("check_pointer",x@ptr)) stop("pointer is NULL, you need to recreate the object")
    .Call("read_sysfile_data",
      x@ptr,
      what=x,
      ncases=n,
      types=x@data.spec$types
)})

setMethod("readSubset","spss.system.importer",
  function(x,rows,cols){
    if(!.Call("check_pointer",x@ptr)) stop("pointer is NULL, you need to recreate the object")
    .Call("read_sysfile_subset",x@ptr,
      what=x,
      j=cols,i=rows,
      types=x@data.spec$types
)})

setMethod("show","spss.system.importer",
  function(object){
    file.name <- attr(object@ptr,"file.name")
    nobs <- nrow(object)
    nvar <- ncol(object)
    varlab.file <- object@varlab.file
    codes.file <- object@codes.file
    missval.file <- object@missval.file
    cat("\nSPSS system file",sQuote(file.name),"\n\twith ")
    cat(nvar,"variables and ")
    cat(nobs,"observations\n")
    if(length(varlab.file)) cat("\twith variable labels from file",sQuote(varlab.file),"\n")
    if(length(codes.file)) cat("\twith value labels from file",sQuote(codes.file),"\n")
    if(length(missval.file)) cat("\twith missing value definitions from file",sQuote(missval.file),"\n")
})

# restore_sysfile_pointer <- function(ptr){
#     file.name <- attr(ptr,"file.name")
#     ptr.attr <- attributes(ptr)
#     ptr <- .Call("NewSysFile",file.name)
#     attributes(ptr) <- ptr.attr
#     warning("restoring ",sQuote(file.name))
#     .Call("restore_sysfile",ptr)
# }