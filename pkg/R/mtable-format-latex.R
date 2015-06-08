mtable_format_latex <- function(x,
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=getOption("digits"),
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = " $\\times$ ",
          sdigits=1,
          drop=TRUE,
          ...
          ){

  colsep <- "&"
  rowsep <- "\n"
  
  coefs <- x$coefficients
  summaries <- x$summaries
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  model.names <- names(coefs)
  
  coef.dims1 <- unique(sapply(coef.dims,"[[",1))
  stopifnot(length(coef.dims1)==1)
  
  coef.names <- dimnames(coefs[[1]])[[3]]
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  modcols <- sapply(coefs,function(x){
    if(length(dim(x))<4) 1
    else dim(x)[4]
  })
  if(drop){
    umc <- unique(modcols)
    if(length(umc)>1) drop <- FALSE
    else
      drop <- drop && umc == 1
  }
  
  totcols <- lapply(coef.dims,"[",-c(3,1))
  totcols <- sapply(totcols,prod)
  totcols <- sum(totcols)
  coef.spec <- character(totcols)
  coef.spec[] <- colspec
  
  tmp <- sdigits
  sdigits <- structure(integer(length(coefs)),names=model.names)
  sdigits[] <- tmp
  
  mtab <- character()
  
  frmt1 <- function(name,coefs,summaries,sdigits){
    
    coef.tab <- ftable(coefs,row.vars=c(3,1))
    coef.tab[] <- sub("(\\*+)","^{\\1}",coef.tab)
    coef.tab[] <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",coef.tab)
    if(!useDcolumn)
      coef.tab[] <- paste0("$",coef.tab,"$")
    name <- paste0("\\multicolumn{",ncol(coef.tab),"}{c}{",name,"}")
    
    if(!drop){
      if(length(dim(coefs))>3 && dim(coefs)[4]>1){
        eq.names <- dimnames(coefs)[[4]]
        mcol <- ncol(coef.tab)/length(eq.names)
        eq.names <- paste0("\\multicolumn{",mcol,"}{c}{",eq.names,"}")
        eq.names <- paste0(eq.names,collapse=colsep)
      }
      else
        eq.names <- ""
    }
    else eq.names <- NULL
    
    if(useDcolumn){
      has.dot <- grep(".",summaries,fixed=TRUE)
      sumry.spec <- if(useDcolumn && has.dot) paste("D{.}{",LaTeXdec,"}{",sdigits,"}",sep="") else "r"
      summaries <- paste0("\\multicolumn{1}{",sumry.spec,"}{",summaries,"}")
    }
    else 
      summaries <- paste0("$",summaries,"$")

    tmp.smry <- summaries
    summaries <- matrix("",nrow=length(tmp.smry),ncol=ncol(coef.tab))
    summaries[,1] <- tmp.smry
    
    coef.tab <- apply(coef.tab,1,paste,collapse=colsep)
    summaries <- apply(summaries,1,paste,collapse=colsep)
    
    as.matrix(c(name,eq.names,coef.tab,summaries))
  }

  tab.spec <- character()
  cmidrule.start <- 0
  cmidrule.end <- 0
  
  for(n in names(coefs)){
    nc <- prod(dim(coefs[[n]])[-c(3,1)])
    tmp.spec <- coef.spec[1:nc]
    tmp.spec <- paste0(tmp.spec,collapse="")
    tab.spec <- c(tab.spec,tmp.spec)
    coef.spec <- coef.spec[-(1:nc)]
    mtab <- cbind(mtab,frmt1(n,coefs[[n]],summaries[,n],sdigits[n]))
    ci <- length(cmidrule.start)
    if(!drop){
      cmidrule.start <- c(cmidrule.start,cmidrule.end[ci]+2)
      cmidrule.end <- c(cmidrule.end,cmidrule.end[ci]+1+nc)
    }
  }
  
  hdrlines <- if(drop) 1 else 1:2
  smrylines <- seq(to=nrow(mtab),length=nrow(summaries))
  
  ldr <- character(length(coef.names)*coef.dims1)
  ii <- seq(from=1,length=length(coef.names),by=coef.dims1)
  ldr[ii] <- coef.names
  ldr <- c(character(length(hdrlines)),ldr,rownames(summaries))

  if(!drop){
    mtab <- apply(mtab,1,paste,collapse=paste0(colsep,colsep))
    tab.spec <- paste0(tab.spec,collapse="c")
  }
  else{
    mtab <- apply(mtab,1,paste,collapse=colsep)
    tab.spec <- paste0(tab.spec,collapse="")
  }
  
  mtab <- paste(ldr,mtab,sep=colsep)
  tab.spec <- paste0("l",tab.spec)
  mtab <- paste0(mtab,"\\\\")
  if(!drop){
    use.cmidrule <- cmidrule.start < cmidrule.end
    if(any(use.cmidrule)){
      cmidrule.start <- cmidrule.start[use.cmidrule]
      cmidrule.end <- cmidrule.end[use.cmidrule]
      cmidrules <- paste0(cmidrule,"{",cmidrule.start,"-",cmidrule.end,"}")
      cmidrules <- paste(cmidrules,collapse="")
      mtab[1] <- paste0(mtab[1],rowsep,cmidrules)
    }
  }
  
  ans <- c(
    toprule,
    if(length(hdrlines)) mtab[hdrlines],
    if(length(hdrlines)) midrule,
    mtab[-c(hdrlines,smrylines)],
    if(length(summaries)) midrule,
    if(length(summaries)) mtab[smrylines],
    bottomrule
  )
  
  tabbegin <- paste("\\begin{tabular}{",tab.spec,"}",sep="")
  tabend <- "\\end{tabular}"
  
  splash <- c("%")
  splash <- c(splash,"% Calls:")
  calls <- x$calls
  for(i in seq(calls)){
    tmp <- paste("% ",names(calls)[i],": ",sep="")
    tmp <- paste(tmp,paste(trimws(deparse(calls[[i]])),collapse=" "),"")
    splash <- c(splash,tmp)
  }
  splash <- c(splash,"%")
  splashrule <- rep("%",max(sapply(splash,nchar),sapply(ans,nchar)))
  splashrule <- paste(splashrule,collapse="")
  splash <- c(splashrule,splash,splashrule)
  ans <- c(splash,tabbegin,ans,tabend)
  ans <- paste0(paste(ans,collapse=rowsep),rowsep)
  return(ans)
}




