porStream <- function(file) .Call("NewPorStream",file)
closePorStream <- function(pstream) .Call("closePorStream",pstream)
readOnePushbackPorStream <- function(pstream) .Call("readOnePushbackPorStream",pstream)
readOnePorStream <- function(pstream) .Call("readOnePorStream",pstream)
seekPorStream <- function(pstream,pos) .Call("seekPorStream",pstream,pos)
tellPorStream <- function(pstream) .Call("tellPorStream",pstream)
lineTellPorStream <- function(pstream) .Call("lineTellPorStream",pstream)
offsetTellPorStream <- function(pstream) .Call("offsetTellPorStream",pstream)
readToSlashPorStream <- function(pstream,maxn) .Call("readToSlashPorStream",pstream,maxn)
readIntPorStream <- function(pstream) .Call("readIntPorStream",pstream)
readDoublePorStream <- function(pstream) .Call("readDoublePorStream",pstream)
readStringPorStream <- function(pstream) .Call("readStringPorStream",pstream)
setTranslationPorStream <- function(pstream,translation) .Call("setTranslationPorStream",pstream,translation)
readDataPorStream <- function(pstream,n,types) .Call("readDataPorStream",pstream,n,types)
readSubsetPorStream <- function(pstream,types,use.vars,use.cases) .Call("readSubsetPorStream",pstream,types,use.vars,use.cases)

readPorStream <- function(pstream,n=1,start=NULL){
  if(missing(start))
    .Call("readPorStream",pstream,n)
  else{
    seekPorStream(pstream,pos=start)
    .Call("readPorStream",pstream,n)
  }
}



readRangePorStream <- function(file,range)
  readPorStream(file,n=diff(range[1:2])+1,start=range[1])


readVarPorStream <- function(pstream,start){
  variable <- list()
  variable$width <- readIntPorStream(pstream)
  variable$name <- readStringPorStream(pstream)
  #print(variable$name)
  #cat("\nVariable ",variable$name," width: ",variable$width)
  variable$printformat <- c(readIntPorStream(pstream), readIntPorStream(pstream), readIntPorStream(pstream))
  variable$writeformat <- c(readIntPorStream(pstream), readIntPorStream(pstream), readIntPorStream(pstream))
  variable$missing$values <- c()
  valid.tags <- c("8","9","A","B","C")
  repeat{
    tag.code <- readOnePushbackPorStream(pstream)
    #cat("=========================================================================\n")
    #cat("=========================================================================\n")
    #cat("Tag code is --",dQuote(tag.code),"--\n")
    if(tag.code %in% valid.tags){
      tag.code <- readOnePorStream(pstream)
    if(tag.code=="8") {
         #cat("Found single missing value tag\n")
         if(variable$width)
            variable$missing$values <- c(variable$missing$values,readStringPorStream(pstream))
          else
            variable$missing$values <- c(variable$missing$values,readDoublePorStream(pstream))
          #if(variable$width) cat("String") else cat("numeric")
          #cat("Values: ",variable$missing$values,"\n")
          }
    if(tag.code=="9") {
         #cat("Found LO THRU x tag\n")
          if(variable$width)
            variable$missing$range <- c("-Inf",readStringPorStream(pstream))
          else
            variable$missing$range <- c(-Inf,readDoublePorStream(pstream))
          #if(variable$width) cat("String") else cat("numeric")
          #cat("Range: ",variable$missing$range,"\n")
          }
    if(tag.code=="A") {
         #cat("Found missing (x THRU HI) tag\n")
          if(variable$width)
            variable$missing$range <- c(readStringPorStream(pstream),"Inf")
          else
            variable$missing$range <- c(readDoublePorStream(pstream),Inf)
          #if(variable$width) cat("String") else cat("numeric")
          #cat("Range: ",variable$missing$range,"\n")
          }
    if(tag.code=="B") {
          #cat("Found missing (x THRU y) tag\n")
          if(variable$width)
            variable$missing$range <- c(readStringPorStream(pstream),readStringPorStream(pstream))
          else
            variable$missing$range <- c(readDoublePorStream(pstream),readDoublePorStream(pstream))
          #if(variable$width) cat("String") else cat("numeric")
          #cat("range: ",variable$missing$range,"\n")
          }
    if(tag.code=="C"){
          #cat("Found variable label tag\n")
          variable$label <- trimws(readStringPorStream(pstream))
          #cat("Variable label",variable$label,"\n")
          }
    }
    else {
      #cat("Probably end of variable record")
      #cat("---- remainder:\n")
      #print(readOnePushbackPorStream(pstream))
      break
      }
  }
  return(variable)
}

readLabPorStream <- function(pstream,types){
  nvar <- readIntPorStream(pstream)
  vars <- character(0)
  for(i in 1:nvar)
    vars <- c(vars,readStringPorStream(pstream))
  #cat("\nVariables: ",paste(vars,collapse=" "))
  nlabs <- readIntPorStream(pstream)
  #cat("\nNumber of labels:",nlabs)
  #cat("\nvars[1]=",vars[1])
  type <- types[[vars[1]]]
  #cat("\nType: ",c("integer","real","character")[type])
  #browser()
  if(type==0){
    labs <- numeric(nlabs)
    namlabs <- character(nlabs)
    for(j in 1:nlabs){
      labs[j] <- as.integer(readDoublePorStream(pstream))
      nlabs[j] <- trimws(readStringPorStream(pstream))
      #cat("\n",labs[j]," ",sQuote(nlabs[j]))
      }
    }
  else if(type==1){
    labs <- numeric(nlabs)
    namlabs <- character(nlabs)
    for(j in 1:nlabs){
      labs[j] <- readDoublePorStream(pstream)
      nlabs[j] <- trimws(readStringPorStream(pstream))
      #cat("\n",labs[j]," ",sQuote(nlabs[j]))
      }
    }
  else {
    labs <- character(nlabs)
    for(j in 1:nlabs){
      labs[j] <- readStringPorStream(pstream)
      nlabs[j] <- trimws(readStringPorStream(pstream))
      #cat("\n",labs[j]," ",sQuote(nlabs[j]))
      }
    }
  names(labs) <- nlabs
  return(list(vars=vars,value.labels=labs))
}

readDocumentPorStream <- function(stream){
  nlines <- readIntPorStream(stream)
  doclines <- character(0)
  for(i in 1:nlines){
    doclines[i] <- readStringPorStream(stream)
  }
  doclines
}

parseHeaderPorStream <- function(stream){
  header <- list()
  ident <- readPorStream(stream,start=40,n=40)
  #if(!length(grep("SPSS PORT",ident,fixed=TRUE))) stop("not a portable file or not ascii encoded")
  header$encoding <- split(ident," ")[[1]][1]
  header$control <- readRangePorStream(stream,200 + c(0,60))
  header$digits <- readRangePorStream(stream,200 + c(64,73))
  header$cletters <- readRangePorStream(stream,200 + c(74,99))
  header$lletters <- readRangePorStream(stream,200 + c(100,125))
  header$symbols <- readRangePorStream(stream,200 + c(126,188))
  header$slash <- readRangePorStream(stream,200 + c(142,142))
  header$translation <- readRangePorStream(stream,200 + c(0,255))
  transl <- setTranslationPorStream(stream,header$translation)
#   if(
#     !identical(header$digits,"0123456789") ||
#     !identical(header$cletters,"ABCDEFGHIJKLMNOPQRSTUVWXYZ") ||
#     !identical(header$lletters,"abcdefghijklmnopqrstuvwxyz") ||
#     !identical(header$slash,"/")
#     ) stop("import of files of this encoding not yet supported")

  seekPorStream(stream,pos=464)
  header$version <- readOnePorStream(stream)
  header$creation.date <- readStringPorStream(stream)
  header$creation.time <- readStringPorStream(stream)
  dictionary <- list()
  valid.tags <- c("1","2","3","4","7","D","E")
  nvlab <- 1
  value.labels <- list()
  document <- NULL
  types <- NULL
  repeat{
    #cat("##########################################################################\n")
    #cat("##########################################################################\n")
    #cat("Reading tag\t")
    tag.code <- readOnePorStream(stream)
    #cat("tag is:",dQuote(tag.code)," -- ")
    #browser()
    if(!(tag.code %in% valid.tags)) break
    if(tag.code=="1") {
            #cat("\nFound product tag")
            header$product <- readStringPorStream(stream)
            }
    if(tag.code=="2") {
            #cat("\nFound author tag")
            header$author <- readStringPorStream(stream)
            }
    if(tag.code=="3") {
            #cat("\nFound subproduct tag")
            header$subproduct <- readStringPorStream(stream)
            }
    if(tag.code=="4") {
            #cat("\nFound number of variables tag")
            header$nvar[1] <- readIntPorStream(stream)
            header$nvar[2] <- readIntPorStream(stream)
            types <- integer()
            }
    if(tag.code=="7") {
            #cat("\nFound variable record tag")
            variable <- readVarPorStream(stream)
            nameidx <- match("name",names(variable))
            #print(variable)
            # Can't rely on the information in the print formats, so always "real" 5.9.07
            #types[variable$name] <- if(variable$width) 2 else 1#if (variable$printformat[3] > 0) 1 else 0
            types[variable$name] <- variable$width
            dictionary[[variable$name]] <- variable[-nameidx]
            }
    if(tag.code=="D") {
            #cat("\nFound value labels tag in line ",lineTellPorStream(stream))
            value.labels <- c(value.labels,list(readLabPorStream(stream,types)))
          }
    if(tag.code=="E") {
             #cat("\nFound document tag")
             document <- paste(readDocumentPorStream(stream),collapse="\n")
           }

  }
  if(length(value.labels)){
    names(value.labels) <- paste("label",seq(1,length(value.labels)),sep="")
  }
  p <- list()
  p$header <- header
  p$dictionary <- dictionary
  p$value.labels <- value.labels
  p$types <- types
  p$document <- document
  if(tag.code == "F"){
    #message("Found data tag, good ...\n")
    p$start.data <- tellPorStream(stream)
  }
  else {
    currpos <- tellPorStream(stream)
    currline <- lineTellPorStream(stream)
    offset <- offsetTellPorStream(stream)
    seekPorStream(stream,pos=currpos-80)
    context <- character(2)
    context[1] <- readPorStream(stream,n=80)
    context[2] <- readPorStream(stream,n=80)
    print(context)
    stop("unknown tag ",dQuote(tag.code)," found in line ",currline," offset ",offset)
  }
  return(p)
}



seekDataPorStream <- function(p){
  start <- p$start.data
  seekPorStream(p$stream,start)
}
