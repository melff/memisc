check.file <- function(filename,error=FALSE,warn=FALSE){
    if(!length(filename) || !nchar(filename)){
        if(error) stop("empty filename",call.=FALSE)
        else if(warn) warning("empty filename",call.=FALSE)
        return(FALSE)
    }
    if(file.access(filename,mode=0)== -1){
        if(warn || error){
            msg <- paste("cannot find",sQuote(basename(filename)),"in",sQuote(dirname(filename)))
            if(error) stop(msg,call.=FALSE)
            if(warn) warning(msg,call.=FALSE)
        }
        return(FALSE)
    }
    else return(TRUE)
}

prettifyLab <- function(x){
      if(!is.character(x)) return(x)
      nx <- names(x)
      x <- gsub("^[0-9]+[.][0-9]*\\s+","",x)
      x <- paste(
                  toupper(substring(x,1,1)),
                  tolower(substring(x,2)),
                  sep=""
                  )
      names(x) <- nx
         return(x)
    }

prettifyNames <- function(x){
      if(!length(x) || !is.atomic(x) && !is.list(x)) return(x)
      nx <- names(x)
      nx <- gsub("^[0-9]+[.][0-9]*\\s+","",nx)
      nx <- paste(
                  toupper(substring(nx,1,1)),
                  tolower(substring(nx,2)),
                  sep=""
                  )
      names(x) <- nx
      return(x)
    }

