setMethod("codebook","data.set",function(x){
  cb <- lapply(x,codebookEntry)
  new("codebook",cb)
})

setMethod("codebook","item",function(x){
  xname <- paste(deparse(substitute(x)))
  cb <- list(codebookEntry(x))
  names(cb) <- xname
  new("codebook",cb)
})

setMethod("codebookEntry","item",function(x){
  annotation <- annotation(x)
  filter <- x@value.filter
  spec <- c(
            "Storage mode:"=storage.mode(x),
            "Measurement:"=measurement(x)
            )
  if(length(filter)) spec <- c(spec,
                              switch(class(filter),
                                        missing.values = c("Missing values:" = format(filter)),
                                        valid.values   = c("Valid values:"   = format(filter)),
                                        valid.range    = c("Valid range:"    = format(filter))
                                        ))
  new("codebookEntry",
    spec = spec,
    stats = switch(measurement(x),
            nominal=,ordinal=codebookStatsCateg(x),
            interval=,ratio=codebookStatsMetric(x)
            ),
    annotation = annotation
  )
})

codebookStatsCateg <- function(x)
    list(tab=Table(x,style="codebook"))



codebookStatsMetric <- function(x){
  tab <- if(length(x@value.labels)) Table(x,style="codebook")
         else NULL
  descr <- Descriptives(x)
  list(
    tab=tab,
    descr=descr
    )
}

setMethod("as.character","codebook",function(x,...){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=x,name=names(x),toprule=toprule,midrule=midrule)
  unlist(out)
})

setMethod("show","codebook",function(object){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=object,name=names(object),toprule=toprule,midrule=midrule)
  out <- unlist(out)
  writeLines(out)
})

# setMethod("Write",signature(x="codebook"),
#           function(x,file=stdout(),...){
#             width <- getOption("width")
#             toprule <- paste(rep("=",width),collapse="")
#             midrule <- paste(rep("-",width),collapse="")
#             out <- mapply(format,x=x,name=names(x),toprule=toprule,midrule=midrule)
#             out <- unlist(out)
#             writeLines(out,con=file)
#           })

Write.codebook <- function(x,file=stdout(),...){
  width <- getOption("width")
  toprule <- paste(rep("=",width),collapse="")
  midrule <- paste(rep("-",width),collapse="")
  out <- mapply(format,x=x,name=names(x),toprule=toprule,midrule=midrule)
  out <- unlist(out)
  writeLines(out,con=file)
}


setMethod("format","codebookEntry",
  function(x,name="",width=getOption("width"),
          toprule=toprule,
          midrule=midrule
      ){

  annot <- x@annotation
  description <- annot["description"]
  wording <- annot["wording"]
  if(length(annot)) annot <- annot[names(annot) %nin% c("description","wording")]

  title <- strwrap(if(length(description) && !is.na(description))
                  paste(name[1],sQuote(description))
                  else name[1] ,width=width,prefix="   ")
  
  wording <- if(length(wording) && !is.na(wording))
                strwrap(dQuote(wording),width=width,prefix="   ")
             else NULL

  spec <- paste("  ",names(x@spec),x@spec)
  tab <- x@stats$tab
  descr <- x@stats$descr
  
  if(length(tab)){
    tab <- cbind(
              formatC(tab[,1,drop=FALSE],format="d"),
              formatC(tab[,2],format="f",digits=1),
              formatC(tab[,3],format="f",digits=1)
            )
    tab[tab=="NA"] <- ""
    tab <- format(tab,justify="right")
    tab <- cbind(
            format(c("Values and labels","",rownames(tab)),justify="right"),
            format(c("N","",tab[,1]),justify="right"),
            " ",
            format(c("Percent","",apply(tab[,2:3,drop=FALSE],1,paste,collapse=" ")),justify="centre")
            )
    tab <- paste("  ",apply(tab,1,paste,collapse=" "))
  }
  if(length(descr)){
    descr <- cbind(
            format(paste(names(descr),": ",sep=""),justify="right"),
            format(formatC(descr,format="f",digits=3),justify="right")
            )
    descr <- paste("  ",apply(descr,1,paste,collapse=" "))
  }
  if(length(tab) && length(descr)){
    statstab <- paste("   ",format(c(tab,"",descr),justify="left"))
  }
  else if(length(tab)){
    statstab <- tab
  }
  else if(length(descr)){
    statstab <- paste("   ",descr)
  }
  
  annot.out <- character()
  if(length(annot)){
    for(i in seq_len(length(annot))){
      annot.i <- annot[i]
      nm.i <- trimws(names(annot.i))
      annot.i <- strwrap(annot.i,width=getOption("width")-8-4)
      annot.i <- c(paste("      ",annot.i),"")
      if(nzchar(nm.i)){
        annot.i <- c(
          paste("   ",nm.i,":",sep=""),
          "",
          annot.i
          )
      }
      annot.out <- c(annot.out,annot.i)
    }
  }
  c(
    toprule,
    "",
    title,
    if(length(wording)) c(
        "",
        wording
        ),
    "",
    midrule,
    "",
    spec,
    "",
    statstab,
    "",
    if(length(annot.out)) annot.out
    )
})