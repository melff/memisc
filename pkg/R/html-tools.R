
show_html <- function(x,output=NULL,...){
  
  ht <- format_html(x,...)
  
  if(interactive()){
    # Test whether running under RStudio 
    isRStudio <- Sys.getenv("RSTUDIO") == "1"
    if(isRStudio)
      deflt.output <- "file-show"
    else
      deflt.output <- "browser"
  }
  else
    deflt.output <- "stdout"
  
  if(missing(output))
    output <- getOption("html_viewer",deflt.output)
  
  if(mode(output)=="character")
      output <- match.arg(output,c("stdout","browser","file-show"))
  else if(!is.function(output))
      stop("'output' should be either a character string of a function")
  
  if(is.function(output)){
    
    tf <- tempfile()
    tf <- paste0(tf,".html")
    cat(ht,file=tf)
    
    output(tf)
  }
  else if(output=="stdout") cat(ht)
  else {
    
    tf <- tempfile()
    tf <- paste0(tf,".html")
    cat(ht,file=tf)
    
    if(output=="file-show")
      file.show(tf,title=deparse(substitute(x)))
    else 
      browseURL(tf)
  }
  
}

write_html <- function(x,file,...)
  cat(format_html(x,...),file=file)

format_html <- function(x,...)
  UseMethod("format_html")



spltDec <- function(x,at="."){
  y <- strsplit(x,at,fixed=TRUE)
  y1 <- sapply(y,"[",1)
  y3 <- sapply(y,"[",2)
  y1[is.na(y1)] <- ""
  y3[is.na(y3)] <- ""
  y2 <- ifelse(grepl("[[:digit:]]+",y3),at,"")
  y <- rbind(y1,y2,y3)
  as.vector(y)
}



upd_vect <- function(x,...){
  y <- c(...)
  n <- names(y)
  x[n] <- y
  x
}
