Stata.file <- function(file,
                       iconv=TRUE,
                       encoded=if(new_format)getOption("Stata.new.encoding","utf-8")
                               else getOption("Stata.old.encoding","cp1252"),
                       negative2missing = FALSE
                       ){
    file <- path.expand(file)
    check.file(file,error=TRUE)

    ptr <- dta117_file_open(file)

    new_format <- dta117_check_magic(ptr)
    
    if(new_format){

        dta117_read_header(ptr)
        dta117_read_map(ptr)
        types <- dta117_read_vtypes(ptr)
        vnames <- dta117_read_vnames(ptr)
        dta117_read_sortlist(ptr)
        dta117_read_formats(ptr)
        vallab_names <- dta117_read_vlab_names(ptr)
        varlabs <- dta117_read_varlabs(ptr)
        vallabs <- dta117_read_vallabs(ptr)
        variables <- dta117_make_prototype(types)

        names(types) <- vnames
        names(variables) <- vnames
        names(varlabs) <- vnames
        names(vallab_names) <- vnames
        
        varlabs <- varlabs[nzchar(varlabs)]
        vallabs <- vallabs[nzchar(vallabs)]
        vallab_names <- vallab_names[nzchar(vallab_names)]

        vallabs <- vallabs[vallab_names]
        names(vallabs) <- names(vallab_names)
        
        if(length(varlabs))
            variables[names(varlabs)] <- mapply("description<-",variables[names(varlabs)],varlabs)
        if(length(vallabs))
            suppressWarnings(variables[names(vallabs)] <- mapply(add_value_labels,
                                                                 variables[names(vallabs)],
                                                                 vallabs))

        missings <- dta117_missing.values(types)
        missings <- missings[sapply(missings,length) > 0]
        if(length(missings))
            variables[names(missings)] <- mapply("missing.values<-",variables[names(missings)],missings)
        if(negative2missing)
            variables <- lapply(variables,`valid.range<-`,c(0,Inf))

        missval_labels <- dta117_missval_labels(types)
        missval_labels <- missval_labels[sapply(missval_labels,length) > 0]

        if(length(missval_labels)){
            missval_labels <- lapply(missval_labels,as,"value.labels")
            meas <- lapply(variables[names(missval_labels)],"measurement")
            suppressWarnings(variables[names(missval_labels)] <- mapply(add_value_labels,
                                                                 variables[names(missval_labels)],
                                                                 missval_labels))
            variables[names(missval_labels)] <- mapply("measurement<-",
                                                       variables[names(missval_labels)],
                                                       meas)
        }

        if(iconv)
            variables <- lapply(variables,Iconv,from=encoded,to="")
        else
            encoded = ""
        
        
        new("Stata_new.importer",
            variables,
            ptr=ptr,
            types=types,
            encoded=encoded
            )
        
    }
    else {

        dta117_file_close(ptr)
        ptr <- new.dta(file)
        data.spec <- get.dictionary.dta(ptr)
        types <- data.spec$types
        variables <- .Call("dta_make_prototype",types)
        names(variables) <- data.spec$names
        varlabs <- data.spec$varlabs
        varlabs <- varlabs[nzchar(varlabs)]
        vallabs <- data.spec$value.labels
        missings <- data.spec$missing.values
        missings <- missings[sapply(missings,length) > 0]
        missval_labels <- data.spec$missval_labels
        missval_labels <- missval_labels[sapply(missval_labels,length) > 0]
        
        if(length(varlabs))
            variables[names(varlabs)] <- mapply("description<-",variables[names(varlabs)],varlabs)
        if(length(vallabs))
            suppressWarnings(variables[names(vallabs)] <- mapply("labels<-",variables[names(vallabs)],vallabs))
        if(length(missings))
            variables[names(missings)] <- mapply("missing.values<-",variables[names(missings)],missings)
        if(negative2missing)
            variables <- lapply(variables,`valid.range<-`,c(0,Inf))

        if(length(missval_labels)){
            missval_labels <- lapply(missval_labels,as,"value.labels")
            meas <- lapply(variables[names(missval_labels)],"measurement")
            suppressWarnings(variables[names(missval_labels)] <- mapply(add_value_labels,
                                                                 variables[names(missval_labels)],
                                                                 missval_labels))
            variables[names(missval_labels)] <- mapply("measurement<-",
                                                       variables[names(missval_labels)],
                                                       meas)
        }

        if(iconv)
            variables <- lapply(variables,Iconv,from=encoded,to="")
        else
            encoded <- ""

        new("Stata.importer",
            variables,
            ptr=ptr,
            data.spec=data.spec,
            encoded=encoded
            )
    }
}

## New Stata formats from 117
setMethod("initialize","Stata_new.importer",function(.Object,
                                                     variables,
                                                     ptr,
                                                     types,
                                                     encoded
                                                     ){
     .Object@.Data <- variables
     .Object@ptr <- ptr
     .Object@types <- types
     .Object@encoded <- encoded
     .Object
})

setMethod("getNobs","Stata_new.importer",
          function(x) dta117_dim(x@ptr)[1])

setMethod("seekData","Stata_new.importer",function(x)
  .Call("dta117_seek_data",x@ptr)
)

setMethod("readData","Stata_new.importer",
  function(x,n)
    iconv_list(.Call("dta117_read_data",
      x@ptr,
      what=x,
      n=n,
      types=x@types),
      encoded=x@encoded))

setMethod("readSlice","Stata_new.importer",
  function(x,rows,cols)
    iconv_list(.Call("dta117_read_slice",x@ptr,
      what=x,
      j=cols,i=rows,
      types=x@types),
      encoded=x@encoded))

setMethod("readChunk","Stata_new.importer",
  function(x,nrows,cols)
    iconv_list(.Call("dta117_read_chunk",x@ptr,
      what=x,
      cols=cols,n=nrows,
      types=x@types),
      encoded=x@encoded))

setMethod("show","Stata_new.importer",
  function(object){
    file.name <- attr(object@ptr,"file.name")
    nobs <- nrow(object)
    nvar <- ncol(object)
    cat("\nStata file",sQuote(file.name),"\n\twith ")
    cat(nvar,"variables and ")
    cat(nobs,"observations\n")
})


## Old Stata formats before 117

setMethod("initialize","Stata.importer",function(.Object,
                                                 variables,
                                                 ptr,
                                                 data.spec,
                                                 encoded
                                                 ){
     .Object@.Data <- variables
     .Object@ptr <- ptr
     .Object@data.spec <- data.spec
     .Object@encoded <- encoded
     .Object
})

setMethod("getNobs","Stata.importer",function(x) x@data.spec$nobs)

setMethod("seekData","Stata.importer",function(x)
  .Call("dta_seek_data",x@ptr)
)

setMethod("readData","Stata.importer",
  function(x,n)
    iconv_list(.Call("dta_read_data",
      x@ptr,
      what=x,
      ncases=n,
      types=x@data.spec$types),
      encoded=x@encoded))

setMethod("readSlice","Stata.importer",
  function(x,rows,cols)
    iconv_list(.Call("dta_read_slice",x@ptr,
      what=x,
      j=cols,i=rows,
      types=x@data.spec$types),
      encoded=x@encoded))

setMethod("readChunk","Stata.importer",
  function(x,nrows,cols)
    iconv_list(.Call("dta_read_chunk",x@ptr,
      what=x,
      cols=cols,n=nrows,
      types=x@data.spec$types),
      encoded=x@encoded))

setMethod("show","Stata.importer",
  function(object){
    file.name <- attr(object@ptr,"file.name")
    nobs <- nrow(object)
    nvar <- ncol(object)
    cat("\nStata file",sQuote(file.name),"\n\twith ")
    cat(nvar,"variables and ")
    cat(nobs,"observations\n")
})

subset.Stata.importer <- subset.importer
subset.Stata_new.importer <- subset.importer
