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
rofseek <- function(f,pos,whence) .Call("rofseek",f,pos=0,whence=0)

gget.pattern <- function(pattern,text){
    if(length(text)>1) warning("using only first element")
    start <- gregexpr(pattern,text[1])[[1]]
    if(all(start < 1)) return(character(0))
    stop <- start + attr(start,"match.length") - 1
    mapply(substr,text,start,stop,USE.NAMES=FALSE)
}


gget.pattern.with.args <- function(pattern,text){
    if(length(text)>1) warning("using only first element")
    start <- gregexpr(pattern,text[1])[[1]]
    if(all(start < 1)) return(character(0))
    stop <- start + attr(start,"match.length") - 1
    start1 <- start + attr(start,"match.length") 
    stop1 <- c(start[-1]-1,nchar(text))
    pats <- mapply(substr,text,start,stop,USE.NAMES=FALSE)
    args <- mapply(substr,text,start1,stop1,USE.NAMES=FALSE)
    list(matches=pats,args=args)
}

get.pattern <- function(pattern,text){
    start <- regexpr(pattern,text)
    stop <- start + attr(start,"match.length") - 1
    start[start < 1] <- Inf
    mapply(substr,text,start,stop,USE.NAMES=FALSE)
}

get.pattern.with.args <- function(pattern,text){
    start <- regexpr(pattern,text)
    stop <- start + attr(start,"match.length") - 1
    start[start < 1] <- Inf
    start1 <- ifelse(attr(start,"match.length")>=1,stop+1,Inf)
    stop1 <- nchar(text)
    pats <- mapply(substr,text,start,stop,USE.NAMES=FALSE)
    args <- mapply(substr,text,start1,stop1,USE.NAMES=FALSE)
    list(matches=pats,args=args)
}

spss.parse.data.spec <- function(file){
    text <- paste(readLines(file,n=-1,warn=FALSE),collapse="\n")
    text <- strsplit(text,"[.]\\s*\n|[.]\\s*$")[[1]]
    has.data.list <- grep("data\\s+list\\s+",text,ignore.case=TRUE)
    if(!length(has.data.list)) stop("could not find 'data list' statement")
    if(length(has.data.list)>1) stop("too many 'data list' statments")
    text <- tolower(text[has.data.list])
    text <- strsplit(text,"/",fixed=TRUE)[[1]]
    header <- text[1]
    text <- text[-1]
    if(length(text)>1) stop("multiline format not yet implemented")
    skip <- gget.pattern("skip\\s*=\\s*[0-9]",header)
    if(length(skip)){
        if(length(skip)>1) stop("to many 'skip' clauses")
        skip <- strsplit(skip,"=",fixed=TRUE)[[1]][2]
        skip <- as.numeric(trimws(skip))
    } else skip <- 0
    text <- gsub("\\(\\s+","(",text)
    text <- gsub("\\s+\\)",")",text)
    text <- gsub("\\s*-\\s*","-",text)
    pa <- gget.pattern.with.args("\\s[a-z][a-z0-9_]*",text)
    variables <- trimws(pa$matches)
    specs <- trimws(pa$args)
    specs <- strsplit(specs,"\\s+")
    format.specs <- sapply(specs,function(x)x[2])
    specs <- sapply(specs,function(x)x[1])
    format.specs[is.na(format.specs)] <- ""
    is.string <- format.specs=="(a)"
    specs <- strsplit(specs,"-",fixed=TRUE)
    start <- sapply(specs,function(x)as.numeric(x[1]))
    stop <- sapply(specs,function(x)as.numeric(ifelse(length(x)>1,x[2],x[1])))
    types <- ifelse(is.string,2,1)
    names(types) <- variables
    list(
      types=types,
      start=start,
      stop=stop,
      skip=skip
    )
}

spss.parse.variable.labels <- function(file){
    text <- paste(readLines(file,n=-1,warn=FALSE),collapse="\n")
    text <- strsplit(text,"[.]\\s*\n|[.]\\s*$")[[1]]
    has.var.lab <- grep("variable\\s+labels\\s+",text,ignore.case=TRUE)
    if(!length(has.var.lab)) stop("could not find 'variable label' statement")
    if(length(has.var.lab)>1) stop("too many 'variable label' statments")
    text <- text[has.var.lab]
    text <- gsub("/","",text,fixed=TRUE)
    text <- gsub("variable\\s+labels\\s+","",text,ignore.case=TRUE)
    text <- strsplit(text,"\"")[[1]]
    ii <- seq_along(text)
    variables <- tolower(text[ii%%2==1])
    variables <- trimws(variables[-length(variables)])
    labels <- text[ii%%2==0]
    names(labels) <- variables
    labels
}

spss.parse.labels <- function(file){
    text <- paste(readLines(file,n=-1,warn=FALSE),collapse="\n")
    text <- strsplit(text,"[.]\\s*\n|[.]\\s*$")[[1]]
    has.val.lab <- grep("value\\s+labels\\s+",text,ignore.case=TRUE)
    if(!length(has.val.lab)) stop("could not find 'value labels' statement")
    if(length(has.val.lab)>1) stop("too many 'value labels' statments")
    text <- text[has.val.lab]
    text <- gsub("value\\s+labels\\s+","",text,ignore.case=TRUE)
    text <- strsplit(text,"\"",fixed=TRUE)[[1]]
    text <- trimws(text)
    ii <- seq_along(text)
    labels <- text[ii%%2==0]
    text <- text[ii%%2==1]
    text <- gsub("\\s+"," ",paste(text,collapse=" "))
    text <- strsplit(text,"\\s*/\\s*")[[1]]
    
    pa <- get.pattern.with.args("^[A-Za-z][A-Za-z0-9_]*\\s+",text)
    variables <- tolower(pa$matches)
    values <- strsplit(pa$args," ")
    values <- lapply(values,as.numeric)
    variables <- trimws(variables)
    names(values) <- variables
    lv <- seq_along(variables)
    rp <- sapply(values,length)
    fc <- rep(lv,rp)
    labels <- split(labels,fc)
    mapply(function(x,y)structure(x,names=y),
                values,labels
                )
}


spss.parse.missing.values <- function(file){
    text <- paste(readLines(file,n=-1,warn=FALSE),collapse="\n")
    text <- strsplit(text,"[.]\\s*\n|[.]\\s*$")[[1]]
    has.miss.val <- grep("missing\\s+values\\s+",text,ignore.case=TRUE)
    if(!length(has.miss.val)) stop("could not find 'missing values' statement")
    if(length(has.miss.val)>1) stop("too many 'missing values' statments")
    text <- text[has.miss.val]
    text <- gsub("missing\\s+values\\s+","",text,ignore.case=TRUE)
    text <- trimws(gsub("\\s+"," ",text))
    text <- strsplit(text,"\\(|\\)")[[1]]
    ii <- seq_along(text)
    variables <- tolower(trimws(text[ii%%2==1]))
    variables <- gsub("/","",variables,fixed=TRUE)
    miss.specs <- tolower(text[ii%%2==0])
    uprange <- suppressWarnings(get.pattern("[0-9]+[.]?[0-9]*\\s+thru\\s+hi[ghest]*",miss.specs))
    miss.specs <- gsub("[0-9]+[.]?[0-9]*\\s+thru\\s+hi[ghest]*","",miss.specs)
    lorange <- suppressWarnings(get.pattern("lo[west]*\\s+thru\\s+[0-9]+[.]?[0-9]*\\s+",miss.specs))
    miss.specs <- gsub("lo[west]*\\s+thru\\s+[0-9]+[.]?[0-9]*\\s+","",miss.specs)
    miss.vals <- lapply(strsplit(miss.specs,",\\s*"),function(x){
      x <- as.numeric(x)
      x[!is.na(x)]
      })
    uprange <- as.numeric(gsub("\\s+thru\\s+hi[ghest]*","",uprange))
    uprange <- lapply(uprange,function(x)if(is.na(x))NULL else c(x,Inf))
    lorange <- as.numeric(gsub("lo[west]*\\s+thru\\s+","",lorange))
    lorange <- lapply(lorange,function(x)if(is.na(x))NULL else c(-Inf,x))
    range <- mapply(c,lorange,uprange)
    ans <- mapply(function(x,y)
                if(length(x)&&length(y))
                  list(values=x,range=y)
                else if(length(x)) list(values=x)
                else if(length(y)) list(range=y)
                else NULL,
                miss.vals,range,SIMPLIFY=FALSE)
    names(ans) <- variables
    ans[sapply(ans,length)>0]
}

