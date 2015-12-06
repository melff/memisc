html <- function(tag,...,.content=NULL){
  stopifnot(length(tag)==1)
  structure(
    list(tag=tag,
         attributes=list(...),
         content=.content),
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
      else
        content <- unlist(lapply(x$content,as.character))
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

setAttribs <- function(x,...){
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


setStyle <- function(x,...){
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

setRangeStyle <- function(x,i,...){
  x[i] <- mapply(setStyle,x[i],...,SIMPLIFY=FALSE)
  x
}

.html <- function(x,tag,...) html(tag=tag,...,.content=x)
.html_code <- function(x,tag,...,vectorize=FALSE){
  if(vectorize)
    structure(lapply(x,.html,tag=tag,...),class="html_code")
  else
    html(tag=tag,...,.content=x)
}

as.html_code <- function(x) structure(list(x),class="html_code")

as.character.html_code <- function(x)paste(unlist(lapply(x,as.character)),collapse="")
print.html_code <- function(x,...) 
  cat(as.character(x),...)

html_td <- function(x,...).html_code(x,tag="td",...)
html_tr <- function(x,...).html_code(x,tag="tr",...)


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

html_td_spltDec <- function(x,...){
  y <- matrix(x,nrow=3)
  y1 <- html_td(y[1,],style=html_beforeDec,...,vectorize=TRUE)
  y2 <- html_td(y[2,],style=html_dotDec,...,vectorize=TRUE)
  y3 <- html_td(y[3,],style=html_afterDec,...,vectorize=TRUE)
  y <- rbind(y1,y2,y3)
  dim(y) <- NULL
  structure(y,class="html_code")
}

html_table <- function(x,...) html(tag="table",...,.content=x)


