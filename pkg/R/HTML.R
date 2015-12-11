html <- function(tag,...,.content=NULL){
  stopifnot(length(tag)==1)
  args <- list(...)
  content <- NULL
  if(length(names(args))){
    has.name <- nzchar(names(args))
    attributes <- args[has.name]
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
         attributes=attributes,
         content=content),
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
  out
}

content <- function(x) x$content
"content<-" <- setContent <- function(x,value){
  x$content <- value
  x
}

attrib <- function(x) 
  structure(x$attributes,class="html_attributes")

"attrib<-" <- function(x,value){
  x$attributes <- value
  x
}
"[<-.html.attributes" <- function(x,i,...,value){
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

html_style <- function(...) as.html_style(c(...))

as.html_style <- function(x){
  if(inherits(x,"html_style")) x
  else structure(as.character(unclass(x)),
                 names=names(x),
                 class="html_style")
}

"[<-.html_style" <- function(x,i,...,value){
  x <- unclass(x)
  x[i] <- as.character(value)
  structure(x,class="html_style")
}
as.character.html_style <- function(x,...){
  n <- names(x)
  paste0(n,": ",x,";")
}
print.html_style <- function(x,...)
  cat(as.character(x),...)


style <- function(x){
  x$attributes$style
}
"style<-" <- function(x,value){
  x$attributes$style <- as.html_style(value)
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
    x$attributes$style <- as.html_style(value)
  }
  x
}


.html <- function(x,tag,...) html(tag=tag,...,.content=x)
.html_group <- function(x,tag,...,vectorize=FALSE){
  if(vectorize)
    structure(lapply(x,.html,tag=tag,...),class="html_group")
  else
    html(tag=tag,...,.content=x)
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
  if(all(.check_html_classes(x))) return(x)
  stop("check failed")
}
.check_html_classes <- function(x){
  is_character <- sapply(x,is.character)
  is_html_elem <- sapply(x,inherits,"html_elem")
  is_html_group <- sapply(x,inherits,"html_group")
  is_character | is_html_elem | is_html_group
}

as.character.html_group <- function(x)paste(unlist(lapply(x,as.character)),collapse="")
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

setAttribs.html_group <- function(x,...){
  x <- lapply(x,setAttribs,...)
  structure(x,class="html_group")
}
setStyle.html_group <- function(x,...){
  x <- lapply(x,setStyle,...)
  structure(x,class="html_group")
}





html_td <- function(x,...).html_group(x,tag="td",...)
html_tr <- function(x,...).html_group(x,tag="tr",...)

html_dt <- function(x,...).html_group(x,tag="dt",...)
html_dd <- function(x,...).html_group(x,tag="dd",...)


html_beforeDec <- html_style("text-align"="right",
                             "margin-right"="0px",
                             "padding-right"="0px",
                             "padding-left"="0.3em")

html_dotDec <- html_style("text-align"="center",
                          "margin-left"="0px",
                          "margin-right"="0px",
                          "padding-right"="0px",
                          "padding-left"="0px",
                          width="1px")

html_afterDec <- html_style("text-align"="left",
                            "margin-left"="0px",
                            "padding-left"="0px",
                            "padding-right"="0.3em")

html_td_spltDec <- function(x,style=character(),...){
  html_beforeDec <- html_style(style,html_beforeDec)
  html_dotDec <- html_style(style,html_dotDec)
  html_afterDec <- html_style(style,html_afterDec)
  y <- matrix(x,nrow=3)
  y1 <- html_td(y[1,],style=html_beforeDec,...,vectorize=TRUE)
  y2 <- html_td(y[2,],style=html_dotDec,...,vectorize=TRUE)
  y3 <- html_td(y[3,],style=html_afterDec,...,vectorize=TRUE)
  y <- mapply(list,y1,y2,y3,SIMPLIFY=FALSE)
  y <- lapply(y,as.html_group)
  structure(y,class="html_group")
}

html_table <- function(x,...) html(tag="table",...,.content=x)
html_p <- function(x,...) html(tag="p",...,.content=x)
html_div <- function(x,...) html(tag="div",...,.content=x)


