dta.byte   <- 251
dta.short  <- 252
dta.long   <- 253
dta.float  <- 254
dta.double <- 255

dta.byte.missrange   <- c(0x64,0x7f)
dta.short.missrange  <- c(0x7fe5,0x7fff)
dta.long.missrange   <- c(0x7fffffe5L,0x7fffffffL)
dta.float.missrange  <- c(2.0^0x7f,Inf)
dta.double.missrange <- c(2.0^0x3ff,Inf)

missnames <- paste(".",c("",letters),sep="")
dta.byte.misslab <- structure(0x64 + 1:27,names=missnames)
dta.short.misslab <- structure(0x7fe4 + 1:27,names=missnames)
dta.long.misslab <- structure(0x7fffffe4L + 1:27,names=missnames)
dta.float.misslab <- structure((1+(0:26)/16^3)*2.0^0x7f,names=missnames)
dta.double.misslab <- structure((1+(0:26)/16^3)*2.0^0x3ff,names=missnames)

new.dta <- function(file) .Call("dta_file_open",file,"rb")
dta.read.version <- function(bf) .Call("dta_read_version",bf)
dta.read.header <- function(bf,lablen).Call("dta_read_header",bf,lablen)
dta.read.descriptors <- function(bf,nvar,len.varn,len.fmt,len.lbl)
        .Call("dta_read_descriptors",bf,nvar,len.varn,len.fmt,len.lbl)
dta.read.varlabs <- function(bf,nvar,len.varlab)
        .Call("dta_read_varlabs",bf,nvar,len.varlab)
dta.read.expansion.fields <- function(bf,shortext)
        .Call("dta_read_expansion_fields",bf,shortext)
# dta.read.data <- function(bf,nobs,types)
#         .Call("dta_read_data",bf,nobs,types)        
dta.read.labels <- function(bf,lbllen,padding)
        .Call("dta_read_labels",bf,lbllen,padding)
# dta.read.chunk <- function(bf,vars,obs,types)
#         .Call("dta_read_chunk",bf,vars,obs,types)
dta.trans.types <- function(types)
        .Call("dta_trans_types",types)
dta.calc.obssize <- function(bf,types)
        .Call("dta_calc_obssize",bf,types)
dta.tell <- function(bf) .Call("dta_ftell",bf)
dta.seek <- function(bf,pos,whence) .Call("dta_fseek",bf,pos,whence)
dta.feof <- function(bf) .Call("dta_feof",bf)
dta.skip.records <- function(bf,n) .Call("dta_skip_records",bf,n)
dta.seek.data <- function(bf) .Call("dta_seek_data",bf)
get.dictionary.dta <- function(bf){
  version <- dta.read.version(bf)
  if(version==105){ ## Stata 5
    version.string <- "Stata 5"
    len.varn <- 8
    len.fmt <- 11
    len.lbl <- 8
    len.varlab <- 31
    ext.short <- TRUE
    conv.types <- FALSE
  }
  else if(version==108){ ## Stata 6
    version.string <- "Stata 6"
    len.varn <- 8
    len.fmt <- 11
    len.lbl <- 8
    len.varlab <- 80
    ext.short <- TRUE
    conv.types <- FALSE
  }
  else if(version==110){ ## Stata 7
    version.string <- "Stata 7"
    len.varn <- 32
    len.fmt <- 11
    len.lbl <- 32
    len.varlab <- 80
    ext.short <- FALSE
    conv.types <- TRUE
  }
  else if(version==111){ ## Stata 7 SE
    version.string <- "Stata 7 SE"
    len.varn <- 32
    len.fmt <- 11
    len.lbl <- 32
    len.varlab <- 80
    ext.short <- FALSE
    conv.types <- FALSE
  }
  else if(version==113){ ## Stata 8
    version.string <- "Stata 8"
    len.varn <- 32
    len.fmt <- 11
    len.lbl <- 32
    len.varlab <- 80
    ext.short <- FALSE
    conv.types <- FALSE
  }
  else if(version %in% c(114,115)){ ## Newer
    version.string <- if(version==114) "Stata 9" else "Stata 10"
    len.varn <- 32
    len.fmt <- 48
    len.lbl <- 32
    len.varlab <- 80
    ext.short <- FALSE
    conv.types <- FALSE
  } else {
    stop("version ",version," not yet supported")
  }
  hdr <- dta.read.header(bf,len.varlab)
  nobs <- hdr$nobs
  nvar <- hdr$nvar
  descriptors <- dta.read.descriptors(bf,
          nvar=nvar,
          len.varn=len.varn,
          len.fmt=len.fmt,
          len.lbl=len.lbl)
  varnames <- descriptors$varlist
  types <- if(conv.types) dta.trans.types(descriptors$typelist)
           else descriptors$typelist
  names(types) <- varnames
  obs_size <- dta.calc.obssize(bf,types)
  varlab <- dta.read.varlabs(bf,
          nvar=nvar,
          len.varlab=len.varlab)
  names(varlab) <- varnames
  dta.read.expansion.fields(bf,ext.short)
  vallabs <- descriptors$lbllist
  names(vallabs) <- varnames
  vallabs <- vallabs[nzchar(vallabs)]
  if(version>105){
    dta.skip.records(bf,nobs)
    vallab.patterns <- list()
    while(!dta.feof(bf))
      vallab.patterns <-  c(vallab.patterns,dta.read.labels(bf,len.lbl,3))
    if(!any(sapply(vallab.patterns,length)>0))
      vallab.patterns <- NULL
  }
  else
    vallab.patterns <- NULL
  if(length(vallab.patterns)){
    vallabs[] <- vallab.patterns[vallabs]
  }
  if(version >= 113){
      missing.values <- dta_missing.values(types)
      missval_labels <- dta_missval_labels(types)
  }
  else {
      missing.values <- NULL
      missval_labels <- NULL
  }
    
  list(names=varnames,
       types=types,
       nobs=nobs,
       nvar=nvar,
       varlabs=varlab,
       value.labels=vallabs,
       missing.values=missing.values,
       missval_labels=missval_labels,
       version.string=version.string
      )
}

dta_missing.values <- function(types){
    nvar <- length(length)
    missing.values <- vector(nvar,mode="list")
    missing.values[types==dta.byte]   <- list(list(range=dta.byte.missrange))
    missing.values[types==dta.short]  <- list(list(range=dta.short.missrange)) 
    missing.values[types==dta.long]   <- list(list(range=dta.long.missrange))
    missing.values[types==dta.float]  <- list(list(range=dta.float.missrange)) 
    missing.values[types==dta.double] <- list(list(range=dta.double.missrange))
    names(missing.values) <- names(types)
    missing.values
}

dta_missval_labels <- function(types){
    nvar <- length(length)
    missval_labels <- vector(nvar,mode="list")
    missval_labels[types==dta.byte]   <- list(dta.byte.misslab)
    missval_labels[types==dta.short]  <- list(dta.short.misslab) 
    missval_labels[types==dta.long]   <- list(dta.long.misslab)
    missval_labels[types==dta.float]  <- list(dta.float.misslab)
    missval_labels[types==dta.double] <- list(dta.double.misslab)
    names(missval_labels) <- names(types)
    missval_labels
}
