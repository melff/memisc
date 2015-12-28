mtable_format_print <- function(x,
          topsep="=",
          bottomsep="=",
          sectionsep="-",
          interaction.sep = " x ",
          center.at=getOption("OutDec"),
          align.integers=c("dot","right","left"),
          padding="  ",
          force.names = FALSE,
          ...
          ){

  colsep <- " "
  rowsep <- "\n"

  coefs <- x$coefficients
  summaries <- x$summaries
  
  num.models <- length(coefs)
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  max.coef.ldim <- max(coef.ldim)
  
  coef.dims1 <- unique(sapply(coef.dims,"[",1))
  stopifnot(length(coef.dims1)==1)
  
  grp.coefs <- max.coef.ldim > 3 
  if(grp.coefs){
    coef.dims4 <- sapply(coef.dims[coef.ldim>3],"[",4)
    grp.coefs <- grp.coefs && any(coef.dims4>1)
  }
  
  coef.names <- dimnames(coefs[[1]])[[3]]
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  mtab <- character()
  align.integers <- match.arg(align.integers)
  
  frmt1 <- function(coefs,summaries){
  
    if(length(dim(coefs))==3){
      coef.tab <- apply(coefs,2,centerAt,
                        at=center.at,
                        integers=align.integers)
      if(length(dim(coef.tab))<2)
        dim(coef.tab) <- c(1,dim(coef.tab))
      coef.tab <- apply(coef.tab,1,paste,collapse=" ")
    }
    else{
      coef.tab <- apply(coefs,c(2,4),centerAt,
                        at=center.at,
                        integers=align.integers)
      if(length(dim(coef.tab))<3)
        dim(coef.tab) <- c(1,dim(coef.tab))
      coef.tab <- apply(coef.tab,c(1,3),paste,collapse=" ")
    }
    if(grp.coefs){
      if(length(dim(coefs))>3 && dim(coefs)[4]>1)
        coef.tab <- rbind(dimnames(coefs)[[4]],coef.tab)
      else
        coef.tab <- rbind(character(ncol(coef.tab)),coef.tab)
      coef.tab <- apply(coef.tab,2,format,justify="centre")
    }
    if(length(dim(coef.tab)))
      coef.tab <- apply(coef.tab,1,paste,collapse=colsep)
    if(grp.coefs && (num.models>1 || force.names)){
      if(length(dim(coefs))>3 && dim(coefs)[4]>1)
        grp.line <- paste(rep(sectionsep,nchar(coef.tab[1])),collapse="")
      else
        grp.line <- paste(rep(" ",nchar(coef.tab[1])),collapse="")
      coef.tab <- c(grp.line,coef.tab)
    }
    
    summaries <- centerAt(summaries,
                          at=center.at,
                          integers=align.integers)
    summaries <- format(summaries)
    as.matrix(format(c(coef.tab,summaries),justify="centre"))
  }
  
  if(length(x$model.groups)){
    for(i in seq_along(x$model.groups)){
      
      mg <- x$model.groups[[i]]
      mtab.m <- character()
      
      for(j in mg){
        mtab.m <- cbind(mtab.m,frmt1(coefs[[j]],summaries[,j]))
      }
      mtab.m <- rbind(names(coefs)[mg],mtab.m)
      mtab.m <- apply(mtab.m,2,format,justify="centre")
      mtab.m <- apply(mtab.m,1,paste,collapse=" ")
      grp.line <- paste(rep(sectionsep,nchar(mtab.m[1])),collapse="")
      mtab.m <- c(names(x$model.groups)[i],grp.line,mtab.m)
      mtab.m <- format(mtab.m, justify="centre")
      mtab <- cbind(mtab,mtab.m)
    }
  }
  else {
    for(i in 1:length(coefs)){
      mtab <- cbind(mtab,frmt1(coefs[[i]],summaries[,i]))
    }    
    if(num.models>1 || force.names)
      mtab <- rbind(names(coefs),mtab)
    mtab <- apply(mtab,2,format,justify="centre")
  }
  
  if(num.models>1 || force.names){
    if(length(x$model.groups))
      hdrlines <- if(grp.coefs) 1:5 else 1:3
    else
      hdrlines <- if(grp.coefs) 1:3 else 1
  }
  else {
    if(length(x$model.groups))
      hdrlines <- if(grp.coefs) 1:3 else 1
    else
      hdrlines <- if(grp.coefs) 1 else integer(0)
  } 

  smrylines <- seq(to=nrow(mtab),length=nrow(summaries))
  
  ldr <- character(length(coef.names)*coef.dims1)
  ii <- seq(from=1,length=length(coef.names),by=coef.dims1)
  ldr[ii] <- coef.names
  ldr <- c(ldr,rownames(summaries))
  ldr <- c(character(length(hdrlines)),ldr)
  ldr <- format(ldr,justify="left")
  
  mtab <- cbind(ldr,mtab)
  mtab <- apply(mtab,1,paste,collapse=paste0(colsep,colsep))
  mtab <- paste0(padding,mtab,padding)
  
  if((any(nchar(topsep)))){
    toprule <- rep(topsep,nchar(mtab[1]))
    toprule <- paste(toprule,collapse="")
  } else
    toprule <- NULL
  if((any(nchar(sectionsep)))){
    secrule <- rep(sectionsep,nchar(mtab[1]))
    secrule <- paste(secrule,collapse="")
  } else
    secrule <- NULL
  if((any(nchar(bottomsep)))){
    botrule <- rep(bottomsep,nchar(mtab[1]))
    botrule <- paste(botrule,collapse="")
  } else
    botrule <- NULL
  
  ans <- c(
    toprule,
    if(length(hdrlines)) mtab[hdrlines],
    if(length(hdrlines)) secrule,
    mtab[-c(hdrlines,smrylines)],
    if(length(summaries)) secrule,
    if(length(summaries)) mtab[smrylines],
    botrule
  )
  ans <- paste0(paste(ans,collapse=rowsep),rowsep)
  return(ans)
}

