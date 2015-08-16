rofile <- function(filename) .Call("rofile",as.character(filename))

readfixed <- function(file,what,nlines,start,stop)
  .Call("readfixed",file,what,nlines,start,stop)

readfixedsubset <- function(file,what,j,i,start,stop)
  .Call("readfixedsubset",file,what,j,i,start,stop)

roreadline <- function(file)
  .Call("rofreadline",file)

# print.rofile <- function(x,...)
#   cat("Read-only file",dQuote(attr(x,"filename")),"\n")

roftell <- function(f) .Call("roftell",f)
rofseek <- function(f,pos=0,whence=0) .Call("rofseek",f,pos=pos,whence=whence)
roeof <- function(f) .Call("roeof",f)
roreset <- function(f) .Call("roreset",f)

spss.parse.data.spec <- function(file){
  
  text <- paste(readLines(file,n=-1,warn=FALSE),collapse=" ")
  
  keyword <- .pattern("data\\s+list\\s+",text,ignore.case=TRUE)
  if(!length(keyword)) return(NULL)
  
  header <- .pattern("/",text,before=TRUE,ignore.case=TRUE)
  text <- .remainder(header)
  
  skip <- .pattern("skip\\s*=\\s*[0-9]+",header,ignore.case = TRUE)
  if(!length(skip)) skip <- 0L
  else {
    
    skip <- .pattern("[0-9]+",skip)
    skip <- as.integer(skip)
  }
  
  varnames <- character()
  start <- integer()
  end <- integer()
  types <- integer()
  
  repeat {
    
    if(grepl("^[ \t\r\n]*[.]",text)){
      #message("found closing dot")
      break
    }
    vn <- .getvarnames(text)
    if(!length(vn)) {
      break
    }
    text <- .remainder(vn)
    
    start1 <- .pattern("^[0-9]+",text)
    text <- .remainder(start1)
    start1 <- as.integer(start1)
    text <- .remainder(.pattern("^\\s*-\\s*",text))
    end1 <- .pattern("^[0-9]+",text)
    text <- .remainder(end1)
    end1 <- as.integer(end1)
    type1 <- .pattern("^[(][aA0-9]+[])]",text) 
    if(!length(type1))
      type1 <- 1
    else {
      text <- .remainder(type1)
      type1 <- .pattern("[aA]",type1)
      if(length(type1))
        type1 <- 2
      else
        type1 <- 1
    }
    
    varnames <- c(varnames,vn)
    types <- c(types,type1)
    start <- c(start,start1)
    end <- c(end,end1)
  }
  names(types) <- varnames
  list(
    types=types,
    start=start,
    stop=end,
    skip=skip
  )
}

spss.parse.variable.labels <- function(file){
  text <- paste(readLines(file,n=-1,warn=FALSE),collapse=" ")
  res <- character()
  repeat{
    
    keyword <- .pattern("variable\\s+labels\\s+",text,ignore.case=TRUE)
    if(!length(keyword)) break
    text <- .remainder(keyword)
    vl <- spss.parse1.variable.labels(text)
    text <- .remainder(vl)
    .remainder(vl) <- NULL
    res <- c(res,vl)
  }
  res
}  

spss.parse1.variable.labels <- function(text){
  
  
  names <- character()
  labels <- character()
  
  # Remove leading slash stuff
  text <- sub("^\\s*/\\s*","",text)
  repeat {
    
    if(grepl("^[ \t\r\n]*[.]",text)){
      #message("found closing dot")
      break
    }
    varnames <- .getvarnames(text)
    text <- .remainder(varnames)
    lab <- .getquoted(text)
    text <- .remainder(lab)
    
    names <- c(names,varnames)
    labels <- c(labels,lab)
    #res <- c(res,list(list(varnames=varnames,vallabs=vallabs)))
    if(nchar(text)<1) break
  }
  
  names(labels) <- names
  structure(labels,
            remainder=text)
}

spss.parse.value.labels <- function(file){
  text <- paste(readLines(file,n=-1,warn=FALSE),collapse=" ")
  res <- list()
  repeat{
    
    keyword <- .pattern("value\\s+labels\\s+",text,ignore.case=TRUE)
    if(!length(keyword)) break
    text <- .remainder(keyword)
    vl <- spss.parse1.value.labels(text)
    text <- .remainder(vl)
    .remainder(vl) <- NULL
    res <- c(res,vl)
  }
  res
}  

spss.parse1.value.labels <- function(text){
  
  res <- list()
  repeat {
    
    if(grepl("^[ \t\r\n]*[.]",text)){
      #message("found closing dot")
      break
    }
    varnames <- .getvarnames(text)
    text <- .remainder(varnames)
    .remainder(varnames) <- NULL
    vallabs <- .getvallabs(text)
    text <- .remainder(vallabs)
    labs <- vallabs$labels
    vallabs <- vallabs$values
    names(vallabs) <- labs
    
    vallabs <- rep(list(vallabs),length(varnames))
    names(vallabs) <- varnames
    res <- c(res,vallabs)
    
    if(nchar(text)<1) break
  }
  structure(res,
            remainder=text)
}


spss.parse.missing.values <- function(file){
  
  text <- paste(readLines(file,n=-1,warn=FALSE),collapse=" ")
  res <- list()
  repeat{
    
    keyword <- .pattern("missing\\s+values\\s+",text,ignore.case=TRUE)
    if(!length(keyword)) break
    text <- .remainder(keyword)
    mv <- spss.parse1.missing.values(text)
    text <- .remainder(mv)
    .remainder(mv) <- NULL
    res <- c(res,mv)
  }
  res
}

spss.parse1.missing.values <- function(text){
  
  missvals <- list()
  
  repeat{
    
    if(grepl("^[ \t\r\n]*[.]",text)){
      #message("found closing dot")
      break
    }
    vn <- .getvarnames(text)
    text <- .remainder(vn)
    if(!length(vn)) break
    
    mvspec <- .pattern("^[(].*?[)]",text)
    if(!length(mvspec)) break
    text <- .remainder(mvspec)
    
    mvspec <- gsub("[()]","",mvspec)
    .remainder(mvspec) <- NULL
    
    if(grepl("thru",mvspec,ignore.case=TRUE)){
      lo <- .pattern("thru",mvspec,before=TRUE,ignore.case=TRUE)
      hi <- .remainder(lo)
      lo <- as.numeric(lo)
      if(!is.finite(lo)) lo <- -Inf
      
      if(grepl(",",hi)){
        hi <- .pattern(",",hi,before=TRUE)
        vals <- .remainder(hi)
        vals <- as.numeric(vals)
      }
      else vals <- NULL
      hi <- as.numeric(hi)
      if(!is.finite(hi)) hi <- Inf  
      range <- c(lo,hi)
    }
    else if(!grepl("\"",mvspec)) {
      range <- NULL
      vals <- numeric()
      for(ii in 1:3){
        v <- .pattern(",",mvspec,before=TRUE)
        mvspec <- .remainder(v)
        vals <- c(vals, as.numeric(v))
        if(!length(mvspec)) break
      }
    }
    else {
      range <- NULL
      vals <- character()
      for(ii in 1:3){
        v <- .pattern(",",mvspec,before=TRUE)
        mvspec <- .remainder(v)
        vals <- c(vals, gsub("\"","",v))
        if(!length(mvspec)) break
      }
    }
    mv <- list(values=vals,range=range)
    
    mv <- rep(list(mv),length(vn))
    names(mv) <- vn
    
    missvals <- c(missvals,mv)
  }
  return(structure(missvals,
                   remainder=text))
}


.getvarnames <- function(text){
  
  varnames <- character(0)
  text <- sub("^\\s*","",text)
  text <- sub("^[/;,]\\s*","",text)
  
  repeat {
    vn.pos <- regexpr("^[A-Za-z][A-Za-z0-9_]*",text)
    if(vn.pos < 0) return(structure(varnames,remainder=text))
    vn.length <- attr(vn.pos,"match.length")
    vn <- substring(text,first=1,last=vn.length)
    varnames <- c(varnames,vn)
    text <- substring(text,first=vn.length+1,last=nchar(text))
    text <- trimws(text,right=FALSE)
    text <- gsub("^[;,][ \t\n\r]*","",text)
  }
}

.getvallabs <- function(text){
  text <- trimws(text,right=FALSE)
  if(grepl("^[\"']",text)) .getvallabs1(text,strings=TRUE)
  else .getvallabs1(text)
}

.getvallabs1 <- function(text,strings=FALSE){
  
  if(strings)
    vals <- character(0)
  else
    vals <- numeric(0)
  labs <- character(0)
  repeat{
    text <- trimws(text,right=FALSE)
    if(grepl("^[/|.|;]",text)) return(structure(
      list(values=vals,labels=labs),
      remainder=text))
    
    if(strings){
      val <- .getquoted(text)
      text <- .remainder(val)
    }
    else{
      val <- .getnotquoted(text)
      text <- .remainder(val)
      val <- as.numeric(val)
    }
    .remainder(val) <- NULL
    lab <- .getquoted(text)
    text <- .remainder(lab)
    .remainder(lab) <- NULL
    
    vals <- c(vals,val)
    labs <- c(labs,lab)
  }
}

.getnotquoted <- function(text){
  text <- trimws(text,right=FALSE)
  if(!nzchar(text)) return(NULL)
  tok.pos <- regexpr("[^ \t\r\n]+",text)
  tok.len <- attr(tok.pos,"match.length")
  tok <- substring(text,first=1,last=tok.len)
  text <- substring(text,first=tok.len+1,last=nchar(text))
  structure(tok,remainder=text)
}

.getquoted <- function(text){
  text <- trimws(text,right=FALSE)
  if(!nzchar(text)) stop("empty string")
  if(!grepl("^[\"']",text)) stop("missing quotation mark")
  qm <- substring(text,first=1,last=1)
  text <- substring(text,first=2)
  qm.pos <- regexpr(qm,text)
  tok <- substring(text,first=1,last=qm.pos-1)
  text <- substring(text,first=qm.pos+1,last=nchar(text))
  structure(tok,remainder=text)
}

.pattern <- function(pattern,text,before=FALSE,...){
  attributes(text) <- NULL
  pat.pos <- regexpr(pattern,text,...)
  if(pat.pos < 1) {
    if(before) return(text)
    else return(NULL)
  }
  pat.len <- attr(pat.pos,"match.len")
  if(before)
    pat.found <- substring(text,first=1,last=pat.pos-1)
  else
    pat.found <- substring(text,first=pat.pos,last=pat.pos+pat.len-1)
  text <- substring(text,first=pat.pos+pat.len,last=nchar(text))
  structure(pat.found,remainder=text)
}

.remainder <- function(x){
  y <- attr(x,"remainder")
  attributes(y) <- NULL
  if(length(y)) sub('^[ \t\r\n]+', '',y)
  else y
}

".remainder<-" <- function(x,value){
  attr(x,"remainder") <- value
  x
}
