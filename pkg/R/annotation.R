## annotation ###########################################################################

setMethod("annotation","ANY",function(x)attr(x,"annotation"))
setMethod("annotation","item",function(x)x@annotation)

setMethod("annotation<-",signature(x="ANY",value="NULL"),function(x,value){
  attr(x,"annotation") <- NULL
  x
})


setMethod("annotation<-",signature(x="ANY",value="character"),function(x,value){
  value <- new("annotation",structure(as.character(value),names=names(value)))
  callGeneric(x,value)
})


setMethod("annotation<-",signature(x="item",value="annotation"),function(x,value){
  x@annotation <- value
  x
})

setMethod("annotation<-",signature(x="ANY",value="annotation"),function(x,value){
  attr(x,"annotation") <- value
  x
})

description <- function(x){
    d <- annotation(x)["description"]
    if(!length(d)) return(NULL)
    if(is.na(d)) NULL else unname(d)
}
setGeneric("description",function(x)standardGeneric("description"))

description.data.frame <- function(x){
    dict <- lapply(x,attr,"label")
    structure(dict,class="descriptions")
}

setMethod("description","data.frame",description.data.frame)
setMethod("description","tbl_df",description.data.frame)

"description<-" <- function(x,value){
  annotation(x)["description"] <- value
  x
}

wording <- function(x){
  wdng <- annotation(x)["wording"]
  if(is.na(wdng)) NULL else unname(wdng)
}

"wording<-" <- function(x,value){
  annotation(x)["wording"] <- value
  x
}

setMethod("show","annotation",function(object){
  if(length(object)){
    annot.out <- character()
    for(i in seq_len(length(object))){
      annot.i <- object[i]
      nm.i <- trimws(names(annot.i))
      annot.i <- strwrap(annot.i,width=getOption("width")-8)
      annot.i <- c(paste("    ",annot.i),"")
      if(nzchar(nm.i)){
        annot.i <- c(
          paste(nm.i,":",sep=""),
          annot.i
          )
      }
      annot.out <- c(annot.out,annot.i)
    }
    writeLines(annot.out)
    }
})

