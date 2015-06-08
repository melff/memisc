mtable_format_print <- function(x,
          topsep="=",
          bottomsep="=",
          sectionsep="-",
          interaction.sep = " x ",
          center.at=getOption("OutDec"),
          align.integers=c("dot","right","left"),
          padding="  ",
          ...
          ){

  colsep <- " "
  rowsep <- "\n"

  coefs <- x$coefficients
  summaries <- x$summaries
  
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
    coef.tab <- ftable(coefs,row.vars=c(3,1))
    coef.tab <- apply(coef.tab,2,centerAt,
                      at=center.at,
                      integers=align.integers)
    if(grp.coefs){
      if(length(dim(coefs))>3 && dim(coefs)[4]>1)
        coef.tab <- rbind(dimnames(coefs)[[4]],coef.tab)
      else
        coef.tab <- rbind(character(ncol(coef.tab)),coef.tab)
      coef.tab <- apply(coef.tab,2,format,justify="centre")
    }
    coef.tab <- apply(coef.tab,1,paste,collapse=colsep)
    if(grp.coefs){
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
  for(n in names(coefs)){
    mtab <- cbind(mtab,frmt1(coefs[[n]],summaries[,n]))
  }
  
  
  mtab <- rbind(names(coefs),mtab)
  mtab <- apply(mtab,2,format,justify="centre")

  hdrlines <- if(grp.coefs) 1:3 else 1
  smrylines <- seq(to=nrow(mtab),length=nrow(summaries))
  
  ldr <- character(length(coef.names)*coef.dims1)
  ii <- seq(from=1,length=length(coef.names),by=coef.dims1)
  ldr[ii] <- coef.names
  ldr <- c(character(length(hdrlines)),ldr,rownames(summaries))
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

