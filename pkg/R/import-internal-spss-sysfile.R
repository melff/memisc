spss.readheader <- function(f)
 .Call("read_sysfile_header",f)

spss.readvars <- function(f,n){
 ans <- list()
  for(i in 1:n){
    currvar <- .Call("read_sysfile_var",f)
    ans <- c(ans,list(currvar))
  }
  names(ans) <- sapply(ans,function(x)x$name)
  ans
}

spss.testcode <- function(f)
  .Call("test_sysfile_int32",f)

spss.readlabels <- function(f){
  ans <- list()
  while(spss.testcode(f)==3){
    ans <- c(ans,list(.Call("read_sysfile_value_labels",f)))
  }
  ans
}

spss.readdocument <- function(f)
  .Call("read_sysfile_document",f)

spss.readaux <- function(f)
  .Call("read_sysfile_aux",f)

spss.dictterm <- function(f)
  .Call("read_sysfile_dict_term",f)

parseSysHeader <- function(file){
  header <- spss.readheader(file)
  swapcode <- header$swap_code
  nvars <- header$case_size
  attr(file,"swap_code") <- swapcode
  attr(file,"case_size") <- nvars
  attr(file,"bias") <- header$bias
  attr(file,"compressed") <- header$compressed
  variables <- spss.readvars(file,nvars)

  value.labels <- NULL
  document <- NULL
  if(spss.testcode(file)==3)
    value.labels <- spss.readlabels(file)
  if(spss.testcode(file)==6)
    document <- spss.readdocument(file)
    
  auxiliaries <- list()
  while(spss.testcode(file)==7){
    aux <- spss.readaux(file)
    auxtype <- aux$type
    aux <- list(aux$data)
    names(aux) <- auxtype
    auxiliaries <- c(auxiliaries,aux)
  }
  if(length(auxiliaries$info_flt64)){

    sysmis  <- auxiliaries$info_flt64["sysmis"]
    highest <- auxiliaries$info_flt64["highest"]
    lowest  <- auxiliaries$info_flt64["lowest"]
  }
  else{

    warning("file lacks info_flt64 record, using defaults")

    info_flt64 <- .Call("dflt_info_flt64",file)
    sysmis  <- info_flt64["sysmis"]
    highest <- info_flt64["highest"]
    lowest  <- info_flt64["lowest"]
  }
  attr(file,"sysmis") <- sysmis
  attr(file,"highest") <- highest
  attr(file,"lowest") <- lowest
  if(spss.testcode(file)==999) start.data <- spss.dictterm(file)
  else stop("did not find dictionary termination code")
  #message("\nstart of data:",p$start.data)
  attr(file,"sysmis") <- sysmis
  attr(file,"data_pos") <- start.data

  if(length(auxiliaries$longVariableNames)){
    longVariableNames <- auxiliaries$longVariableNames
    longVariableNames <- strsplit(longVariableNames,"\t")[[1]]
    longVariableNames <- strsplit(longVariableNames,"=")
    longVariableNames <- sapply(longVariableNames,function(lvn)
        structure(lvn[2],names=lvn[1]))
    ii <- match(names(longVariableNames),names(variables))
    names(variables)[ii] <- unname(longVariableNames)
  }
  
  variables <- lapply(variables,function(x)x[-1])
  
  missings <- list()
  
  missings$values <- lapply(variables,"[[","missings")
  missings$ranges <- vector(length(variables),mode="list")
  names(missings$ranges) <- names(missings$values)
  n_missings <- sapply(variables,"[[","n_missing_values")
  
  mrang <- missings$values[n_missings == -2]
  if(length(mrang)){
    mrang <- do.call(rbind,mrang)
    is.lo <- mrang[,1] == lowest
    is.hi <- mrang[,2] == highest
    if(length(is.lo))
      mrang[is.lo,1] <- -Inf
    if(length(is.hi))
      mrang[is.hi,2] <- Inf
    missings$ranges[n_missings == -2] <- split(mrang,row(mrang))
  }
  mrang_val <- missings$values[n_missings == -3]
  if(length(mrang_val)){
    mrang_val <- do.call(rbind,mrang_val)
    
    is.lo <- mrang_val[,1] == lowest
    is.hi <- mrang_val[,2] == highest
    if(length(is.lo))
      mrang_val[is.lo,1] <- -Inf
    if(length(is.hi))
      mrang_val[is.hi,2] <- Inf
    missings$values[n_missings == -3] <- split(unname(mrang_val[,3]),seq_len(nrow(mrang_val)))
    missings$ranges[n_missings == -3] <- split(mrang_val[,1:2],row(mrang_val[,1:2]))
  }
  missings$values[n_missings == -2] <- list(NULL)
  
  types <- sapply(variables,function(x)x$type)
  digits <- sapply(variables,function(x)x$print[4])
  list(
    header = header,
    auxiliaries = auxiliaries,
    variables = variables[types>=0],
    value.labels = value.labels,
    missings = missings,
    types = types,
    document = document
    )
}

# seekSysData <- function(p)
#   .Call("rewind_sysfile",p$file)

