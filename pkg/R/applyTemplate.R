allequal <- function(x) length(unique(x)) == 1

get.substr <- function(pattern, text, perl = FALSE,
        fixed = FALSE, useBytes = FALSE){
        if(fixed)
          rr <- gregexpr(pattern,text,fixed=TRUE,perl=FALSE,useBytes=useBytes)
        else
          rr <- gregexpr(pattern,text,perl=perl,useBytes=useBytes)
        positions <- lapply(rr,function(rr){
          if(rr[1]<0) return(NULL)
          len <- attr(rr,"match.length")
          sapply(1:length(rr),function(i){
          start <- rr[i]
          stop <- rr[i] + len[i] - 1
          c(start=start,stop=stop)
          })})
        ans <- lapply(1:length(text),function(i){
          txt <- text[i]
          pos <- positions[[i]]
          if(!length(pos)) return(NULL)
          sapply(1:NCOL(pos),function(j){
            substr(txt,start=pos["start",j],stop=pos["stop",j])
          })
        })
        structure(ans,positions=positions)
        }

get.oneSubstr <- function(pattern,text){
  if(length(text)>1) {
    warning("only first element used")
    text <- text[1]
    }
  start <- regexpr(pattern,text)
  if(start < 0) return(NULL)
  stop <- start + attr(start,"match.length") - 1
  substr(text,start=start,stop=stop)
}


formatSigSymbols <- function(x,signif.symbols=getOption("signif.symbols")){
  if(length(x)>1) {
    warning("only first element used")
    x <- x[1]
    }
  signif.symbols <- sort(signif.symbols)
  sel <- x <= signif.symbols
  if(!any(sel)) return("")
  sel <- which(signif.symbols == min(signif.symbols[sel]))
  return(names(signif.symbols)[sel])
}

formatOne <- function(x,spec,format="f",
                      digits=min(3,getOption("digits")),
                      signif.symbols=getOption("signif.symbols")
                      ){
  if(length(x)>1) {
    warning("only first element used")
    x <- x[1]
    }
  if(is.na(x)) return("")
  if(length(spec)>1) {
    warning("only first spec used")
    spec <- spec[1]
    }
  if(is.character(x)) return(x)
  if(!is.numeric(x)) {
    x <- paste(x,collapse=" ")
    if(nchar(x)>20)
      x <- paste(substr(x,1,20),"...")
    return(x)
    }
  format.arg <- format
  digits.arg <- digits
  format <- get.oneSubstr("[d-gEG]+",spec)
  digits <- as.numeric(get.oneSubstr("[0-9]+",spec))
  hash <- length(get.oneSubstr("[#]",spec)) > 0
  star <- length(get.oneSubstr("[*]",spec)) > 0
  if(star) return(formatSigSymbols(x,signif.symbols))
  if(hash){
    if(length(digits))
      digits <- min(digits,digits.arg)
    else
      digits <- digits.arg
  }
  if(!length(digits)) digits <- digits.arg
  if(!length(format)) format <- format.arg
  if(!(format %in% c("d","e","f","g","fg","E","G","s"))) stop("illegal format specifier")
  return(formatC(x,digits=digits,format=format,width=-1))
}

formatVec <- function(x,formats,
                      default="f",
                      digits=min(3,getOption("digits")),
                      signif.symbols=getOption("signif.symbols")){
  i <- 1:length(formats)
  
  res <- sapply(i,function(i) if(length(formats[[i]]))
    sapply(formats[[i]],formatOne,
           x=x,
           format=default,
           digits=digits,
           signif.symbols = signif.symbols)
      else NULL
      )
}

get.format <- function(x){
  if(!length(x)) return(NULL)
  x <- strsplit(x,":",fixed=TRUE)
  sapply(x,function(x)sub(")","",x[2],fixed=TRUE))
}


applyTemplate <- function(x,template,float.style=getOption("float.style"),
                      digits=min(3,getOption("digits")),
                      signif.symbols=getOption("signif.symbols")){
   template <- as.matrix(template)
   if(is.numeric(x))
    patterns  <- paste("\\(\\$",1:length(x),":[d-gEG]*[0-9]*[#]?[*]?\\)",sep="")
   else
    patterns  <- paste("\\(\\$",1:length(x),"\\)",sep="")
   names(patterns) <- as.character(1:length(x))
   if(length(names(x))){
        if(is.numeric(x))
          npatterns <- paste("\\(\\$",names(x),":[d-gEG]*[0-9]*[#]?[*]?\\)",sep="")
        else
          npatterns <- paste("\\(\\$",names(x),"\\)",sep="")
        names(npatterns) <- names(x)
        }
   else npatterns <- c()
   res <- template
   for(i in 1:length(patterns)){
      targets <- get.substr(patterns[i],template)
      positions <- attr(targets,"positions")
      formats <- lapply(targets,get.format)
      formatted <- formatVec(x[i],formats,
                             default=float.style,
                             digits=digits,
                             signif.symbols=signif.symbols)
      for(j in 1:length(template)){
        if(length(targets[[j]])){
          targets.j <- targets[[j]]
          formatted.j <- formatted[[j]]
          for(k in 1:length(targets.j))
            res <- gsub(targets.j[k],formatted.j[k],res,fixed=TRUE)
        }
      }
   }
   if(length(npatterns)){
      for(n in names(npatterns)){
          targets <- get.substr(npatterns[n],template)
          positions <- attr(targets,"positions")
          formats <- lapply(targets,get.format)
          formatted <- formatVec(x[n],
                                 formats,
                                 default=float.style,
                                 digits=digits,
                                 signif.symbols=signif.symbols)
          for(j in 1:length(template)){
            if(length(targets[[j]])){
              targets.j <- targets[[j]]
              formatted.j <- formatted[[j]]
              for(k in 1:length(targets.j))
                res <- gsub(targets.j[k],formatted.j[k],res,fixed=TRUE)
            }
          }
      }
   }
   unchanged <- res == template
   res[unchanged] <- ""
   res
}

