html <- function(tag,...,.content=NULL,linebreak=FALSE){
  stopifnot(length(tag)==1)
  args <- list(...)
  content <- NULL
  attribs <- NULL
  if(length(names(args))){
    has.name <- nzchar(names(args))
    attribs <- args[has.name]
    if(any(!has.name) && length(.content))
      stop("use either unnamed arguments or the .content argument")
    if(any(!has.name))
      content <- args[!has.name]
    if(length(.content))
      content <- .content
  }
  else{
    if(length(args)&&length(.content))
      stop("use either unnamed arguments or the .content argument")
    if(length(args))
      content <- args
    else
      content <- .content
  }
  structure(
    list(tag=tag,
         attributes=attribs,
         content=content,
         linebreak=linebreak),
    class="html_elem")
}

print.html_elem <- function(x,...) 
  cat(as.character(x),...)

as.character.html_elem <- function(x,...){
  out <- paste0("<",x$tag)
  if(length(names(x$attributes))){
    for(n in names(x$attributes)){
      a <- x$attributes[[n]]
      a <- as.character(a)
      a <- paste(a,collapse=" ")
      out <- paste(out,paste0(n,"=",shQuote(a,"cmd")))
    }
  }
  out <- paste0(out,">")
  if(length(x$linebreak)>1 && x$linebreak[1]){
    out <- paste0(out,"\n")
  }
  if(length(x$content)){
    if(is.list(x$content)){
      if(inherits(x$content,"html_elem"))
        content <- as.character(x$content)
      else{
        content <- lapply(x$content,as.character)
        content <- unlist(content,as.character)
      }
    }
    else
      content <- as.character(x$content)

    out <- paste0(out,paste(content,collapse=""))
    out <- paste0(out,"</",x$tag,">")
  }
  if(length(x$linebreak)>1){
    if(x$linebreak[2])
      out <- paste0(out,"\n")
  }
  else if(x$linebreak)
    out <- paste0(out,"\n")
  out
}

content <- function(x) x$content
"content<-" <- setContent <- function(x,value){
  x$content <- value
  x
}

attribs <- function(x) 
  structure(x$attributes,class="html_attributes")

"attribs<-" <- function(x,value){
  x$attributes <- value
  x
}
"[<-.html_attributes" <- function(x,i,...,value){
  x <- unclass(x)
  x[i] <- list(value)
  structure(x,class="html_attributes")
}

setAttribs <- function(x,...)UseMethod("setAttribs")
setAttribs.character <- function(x,...)return(x)
setAttribs.html_elem <- function(x,...){
  value <- c(...)
  n <- names(value)
  x$attributes[n] <- value
  x
}

c.html_elem <- function(...) reduce(list(...),join_html)



css <- function(...) as.css(c(...))

as.css <- function(x){
  if(inherits(x,"css")) x
  else structure(as.character(unclass(x)),
                 names=names(x),
                 class="css")
}

"[<-.css" <- function(x,i,...,value){
  x <- unclass(x)
  x[i] <- as.character(value)
  structure(x,class="css")
}
as.character.css <- function(x,...){
  n <- names(x)
  paste0(n,": ",x,";")
}
print.css <- function(x,...)
  cat(as.character(x),...)


style <- function(x){
  x$attributes$style
}
"style<-" <- function(x,value){
  x$attributes$style <- as.css(value)
  x
}

setStyle <- function(x,...)UseMethod("setStyle")
setStyle.character <- function(x,...)return(x)
# setStyle.list <- function(x,...)lapply(x,setStyle,...)
setStyle.html_elem <- function(x,...){
  value <- c(...)
  s <- x$attributes$style
  if(length(s)){
    n <- names(value)
    s[n] <- value
    x$attributes$style <- s
  }
  else {
    x$attributes$style <- as.css(value)
  }
  x
}


.html <- function(x,tag,...,linebreak=FALSE) html(tag=tag,...,.content=x,linebreak=linebreak)
.html_group <- function(x,tag,...,vectorize=FALSE,linebreak=FALSE){
  if(vectorize)
    structure(lapply(x,.html,tag=tag,...,linebreak=linebreak),class="html_group")
  else
    html(tag=tag,...,.content=x,linebreak=linebreak)
}

as.html_group <- function(x) structure(check_html_classes(x),class="html_group")
html_group <- function(...){ 
  x <- list(...)
  x <- x[sapply(x,length)>0]
  as.html_group(x)
  }
check_html_classes <- function(x){
  if(inherits(x,"html_elem")) return(x)
  if(inherits(x,"html_group")) return(x)
  if(is.character(x)) return(x)
  if(!is.list(x)) stop("check failed")
  if(all(sapply(x,.check_html_classes))) return(x)
  stop("check failed")
}
.check_html_classes <- function(x){
  if(inherits(x,"html_elem")) return(TRUE)
  if(inherits(x,"html_group")) return(TRUE)
  if(is.character(x)) return(TRUE)
  if(is.list(x)) return(all(sapply(x,.check_html_classes)))
  else return(FALSE)
}

as.character.html_group <- function(x,...)
  paste(unlist(lapply(x,as.character)),collapse="")
print.html_group <- function(x,...) 
  cat(as.character(x),...)

"[.html_group" <- function(x,i,....){
  x <- NextMethod()
  structure(x,class="html_group")
}
"[<-.html_group" <- function(x,i,....,value){
  x <- NextMethod()
  structure(x,class="html_group")
}

c.html_group <- function(...) reduce(list(...),join_html)

setAttribs.html_group <- function(x,...){
  x <- lapply(x,setAttribs,...)
  structure(x,class="html_group")
}
setStyle.html_group <- function(x,...){
  x <- lapply(x,setStyle,...)
  structure(x,class="html_group")
}

join_html <- function(x,y){

  if(inherits(x,"html_elem")&&inherits(y,"html_elem"))
    return(structure(list(x,y),class="html_group"))
  else if(inherits(x,"html_group")&&inherits(y,"html_elem"))
    return(structure(c(unclass(x),list(y)),class="html_group"))
  else if(inherits(x,"html_elem")&&inherits(y,"html_group"))
    return(structure(c(list(x),unclass(y)),class="html_group"))
  else if(inherits(x,"html_group")&&inherits(y,"html_group"))
    return(structure(c(unclass(x),unclass(y)),class="html_group"))
  else stop("cannot handle these arguments.")
}



html_td <- function(x,...,linebreak=FALSE).html_group(x,tag="td",...,linebreak=linebreak)
html_tr <- function(x,...,linebreak=TRUE).html_group(x,tag="tr",...,linebreak=linebreak)

html_dt <- function(x,...,linebreak=TRUE).html_group(x,tag="dt",...,linebreak=linebreak)
html_dd <- function(x,...,linebreak=TRUE).html_group(x,tag="dd",...,linebreak=linebreak)


html_beforeDec <- css("text-align"="right",
                             "margin-right"="0px",
                             "padding-right"="0px",
                             "padding-left"="0.3em")

html_dotDec <- css("text-align"="center",
                          "margin-left"="0px",
                          "margin-right"="0px",
                          "padding-right"="0px",
                          "padding-left"="0px",
                          width="1px")

html_afterDec <- css("text-align"="left",
                            "margin-left"="0px",
                            "padding-left"="0px",
                            "padding-right"="0.3em")

html_td_spltDec <- function(x,style=character(),...,linebreak=FALSE){
  html_beforeDec <- css(style,html_beforeDec)
  html_dotDec <- css(style,html_dotDec)
  html_afterDec <- css(style,html_afterDec)
  y <- matrix(x,nrow=3)
  y1 <- html_td(y[1,],style=html_beforeDec,...,vectorize=TRUE)
  y2 <- html_td(y[2,],style=html_dotDec,...,vectorize=TRUE)
  y3 <- html_td(y[3,],style=html_afterDec,...,vectorize=TRUE,linebreak=linebreak)
  y <- mapply(list,y1,y2,y3,SIMPLIFY=FALSE)
  y <- lapply(y,as.html_group)
  structure(y,class="html_group")
}

html_table <- function(x,...,linebreak=c(TRUE,TRUE)) html(tag="table",...,.content=x,linebreak=linebreak)
html_p <- function(x,...,linebreak=TRUE) html(tag="p",...,.content=x,linebreak=linebreak)
html_div <- function(x,...,linebreak=c(TRUE,TRUE)) html(tag="div",...,.content=x,linebreak=linebreak)


format_html.html_elem <- function(x,...) as.character.html_elem(x)
format_html.html_group <- function(x,...) as.character.html_group(x)

