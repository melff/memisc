mtable_format_latex <- function(x,
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=min(3,getOption("digits")),
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = " $\\times$ ",
          sdigits=min(1,ddigits),
          force.names = FALSE,
          compact=FALSE,
          sumry.multicol=FALSE,
          ...
          ){

  colsep <- "&"
  rowsep <- "\n"
  
  coefs <- x$coefficients
  summaries <- x$summaries
  
  num.models <- length(coefs)
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  max.coef.ldim <- max(coef.ldim)
  
  coef.dims1 <- unique(sapply(coef.dims,"[[",1))
  stopifnot(length(coef.dims1)==1)
  
  coef.names <- dimnames(coefs[[1]])[[3]]
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  totcols <- lapply(coef.dims,"[",-c(3,1))
  totcols <- sapply(totcols,prod)
  totcols <- sum(totcols)
  coef.spec <- character(totcols)
  coef.spec[] <- colspec
  
  tmp <- sdigits
  sdigits <- structure(integer(length(coefs)),names=names(coefs))
  sdigits[] <- tmp
  
  grp.coefs <- max.coef.ldim > 3 
  if(grp.coefs){
    coef.dims4 <- sapply(coef.dims[coef.ldim>3],"[",4)
    grp.coefs <- grp.coefs && any(coef.dims4>1)
  }
  
  mtab <- character()
  
  frmt1 <- function(name,coefs,summaries,sdigits){
    
    coef.tab <- coefs
    dm <- dim(coefs)
    if(length(dm)==3) dm <- c(dm,1)
    dim(coef.tab) <- dm
    coef.tab <- aperm(coef.tab,c(1,3,2,4))
    dim(coef.tab) <- c(dm[1]*dm[3],dm[2]*dm[4])

    coef.tab[] <- sub("(\\*+)","^{\\1}",coef.tab)
    coef.tab[] <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",coef.tab)
    if(!useDcolumn)
      coef.tab[] <- paste0("$",coef.tab,"$")
      
    if(grp.coefs){
      hdr <- character(ncol(coef.tab))
      if(dm[4]>1){
        eq.names <- dimnames(coefs)[[4]]
        eq.names <- paste0("\\multicolumn{",dm[2],"}{c}{",eq.names,"}")
        eq.names <- paste0(eq.names,collapse=colsep)
      }
      else
        eq.names <- ""
    }      
    else eq.names <- NULL
      
    if(num.models>1 || force.names)
      name <- paste0("\\multicolumn{",ncol(coef.tab),"}{c}{",name,"}")
    else
      name <- NULL
    
    if(!useDcolumn)
      summaries <- paste0("$",summaries,"$")
    else if(sumry.multicol){
        has.dot <- grep(".",summaries,fixed=TRUE)
        sumry.spec <- if(useDcolumn && has.dot) paste("D{.}{",LaTeXdec,"}{",sdigits,"}",sep="") else "r"
        summaries <- paste0("\\multicolumn{1}{",sumry.spec,"}{",summaries,"}")
    }

    sum.tab <- matrix("",nrow=length(summaries),ncol=ncol(coef.tab))
    sum.tab[,1] <- summaries
    
    coef.tab <- apply(coef.tab,1,paste,collapse=colsep)
    sum.tab <- apply(sum.tab,1,paste,collapse=colsep)
    c(name,eq.names,coef.tab,sum.tab)
  }

  tab.spec <- character()
  cmidrule.start <- 0
  cmidrule.end <- 0

  cmidrule0.start <- 0
  cmidrule0.end <- 0
  
  if(length(x$model.groups)){
  
    for(i in seq_along(x$model.groups)){

      mg <- x$model.groups[[i]]
      mtab.m <- character()
      tab.spec.m <- character()
      nc0 <- 0
      for(j in mg){
        nc <- prod(dim(coefs[[j]])[-c(3,1)])
        nc0 <- nc0 + nc
        tmp.spec <- coef.spec[1:nc]
        tmp.spec <- paste0(tmp.spec,collapse="")
        tab.spec.m <- c(tab.spec.m,tmp.spec)
        coef.spec <- coef.spec[-(1:nc)]
        mtab.m <- cbind(mtab.m,frmt1(names(coefs)[j],coefs[[j]],summaries[,j],sdigits[j]))
        ci <- length(cmidrule.start)
        cmidrule.start <- c(cmidrule.start,cmidrule.end[ci]+1)
        cmidrule.end <- c(cmidrule.end,cmidrule.end[ci]+nc)
      }
      mtab.m <- apply(mtab.m,1,paste,collapse=colsep)
      group.name <- names(x$model.groups)[i]
      group.name <- paste0("\\multicolumn{",nc0,"}{c}{",group.name,"}")
      mtab.m <- c(group.name,mtab.m)
      mtab <- cbind(mtab,mtab.m)
      tab.spec.m <- paste0(tab.spec.m,collapse="")
      tab.spec <- c(tab.spec,tab.spec.m)
      ci0 <- length(cmidrule0.start)
      cmidrule0.start <- c(cmidrule0.start,cmidrule0.end[ci0]+2)
      cmidrule0.end <- c(cmidrule0.end,cmidrule0.end[ci0]+1+nc0)
    }
  }
  else {
    for(i in 1:length(coefs)){
      nc <- prod(dim(coefs[[i]])[-c(3,1)])
      tmp.spec <- coef.spec[1:nc]
      tmp.spec <- paste0(tmp.spec,collapse="")
      tab.spec <- c(tab.spec,tmp.spec)
      coef.spec <- coef.spec[-(1:nc)]
      mtab <- cbind(mtab,frmt1(names(coefs)[i],coefs[[i]],summaries[,i],sdigits[i]))
      ci <- length(cmidrule.start)
      if(compact){
        cmidrule.start <- c(cmidrule.start,cmidrule.end[ci]+1)
        cmidrule.end <- c(cmidrule.end,cmidrule.end[ci]+nc)
      }
      else{
        cmidrule.start <- c(cmidrule.start,cmidrule.end[ci]+2)
        cmidrule.end <- c(cmidrule.end,cmidrule.end[ci]+1+nc)
      }
    }
  }
  
  smrylines <- seq(to=nrow(mtab),length=nrow(summaries))
  
  ldr <- character(length(coef.names)*coef.dims1)
  ii <- seq(from=1,length=length(coef.names),by=coef.dims1)
  ldr[ii] <- coef.names

  hldr <- NULL
  nhdrl <- 0
  if(num.models>1 || force.names){
    hldr <- c(hldr,"")
    nhdrl <- nhdrl + 1
  }
  if(grp.coefs){
    hldr <- c(hldr,"")
    nhdrl <- nhdrl + 1
  }  
  if(length(x$model.groups)){
    hldr <- c(hldr,"")
    nhdrl <- nhdrl + 1
  }  
  if(nhdrl) 
    hdrlines <- 1:nhdrl
  else
    hdrlines <- 0
  
  ldr <- c(hldr,ldr,rownames(summaries))

  if(compact){
    mtab <- apply(mtab,1,paste,collapse=colsep)
    tab.spec <- paste0(tab.spec,collapse="")
  }
  else {
    mtab <- apply(mtab,1,paste,collapse=paste0(colsep,colsep))
    tab.spec <- paste0(tab.spec,collapse="c")
  }
  
  mtab <- paste(ldr,mtab,sep=colsep)
  tab.spec <- paste0("l",tab.spec)
  mtab <- paste0(mtab,"\\\\")
  
  cmidrule.pos <- 1
  
  if(length(x$model.groups)){
      
      use.cmidrule0 <- cmidrule0.start < cmidrule0.end
      if(any(use.cmidrule0)){
        cmidrule0.start <- cmidrule0.start[use.cmidrule0]
        cmidrule0.end <- cmidrule0.end[use.cmidrule0]
        cmidrules0 <- paste0(cmidrule,"{",cmidrule0.start,"-",cmidrule0.end,"}")
        cmidrules0 <- paste(cmidrules0,collapse="")
        mtab[cmidrule.pos] <- paste0(mtab[cmidrule.pos],rowsep,cmidrules0)
      }
      
      cmidrule.pos <- cmidrule.pos + 1
  }
  
  if(num.models>1 && grp.coefs){
    use.cmidrule <- cmidrule.start < cmidrule.end
    if(any(use.cmidrule)){
      cmidrule.start <- cmidrule.start[use.cmidrule]
      cmidrule.end <- cmidrule.end[use.cmidrule]
      cmidrules <- paste0(cmidrule,"{",cmidrule.start,"-",cmidrule.end,"}")
      cmidrules <- paste(cmidrules,collapse="")
      mtab[cmidrule.pos] <- paste0(mtab[cmidrule.pos],rowsep,cmidrules)
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




