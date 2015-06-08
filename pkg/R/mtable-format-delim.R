mtable_format_delim <- function(x,
                                colsep="\t",
                                rowsep="\n",
                                interaction.sep = " x ",
                                ...
){
  
  coefs <- x$coefficients
  summaries <- x$summaries
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  max.coef.ldim <- max(coef.ldim)
  
  coef.dims1 <- unique(sapply(coef.dims,"[[",1))
  stopifnot(length(coef.dims1)==1)
  
  coef.names <- dimnames(coefs[[1]])[[3]]
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  mtab <- character()
  
  frmt1 <- function(name,coefs,summaries){
    coef.tab <- ftable(coefs,row.vars=c(3,1))
    
    if(max.coef.ldim>3){
      hdr <- character(ncol(coef.tab))
      if(length(dim(coefs))>3){
        eq.names <- dimnames(coefs)[[4]]
        ii <- seq(from=1,length=length(eq.names),by=ncol(coef.tab)%%length(eq.names))
        hdr[ii] <- eq.names
        }
      coef.tab <- rbind(hdr,coef.tab)
    }
    hdr <- character(ncol(coef.tab))
    hdr[1] <- name
    coef.tab <- rbind(hdr,coef.tab)
    if(length(summaries)){
      sum.tab <- matrix("",nrow=length(summaries),ncol=ncol(coef.tab))
      sum.tab[,1] <- summaries
      coef.tab <- rbind(coef.tab,sum.tab)
    }
    coef.tab <- apply(coef.tab,1,paste,collapse=colsep)
  }
  for(n in names(coefs)){
    mtab <- cbind(mtab,frmt1(n,coefs[[n]],summaries[,n]))
  }
  
  hdrlines <- seq.int(max(coef.ldim)-2)
  smrylines <- seq(to=nrow(mtab),length=nrow(summaries))
  
  ldr <- character(length(coef.names)*coef.dims1)
  ii <- seq(from=1,length=length(coef.names),by=coef.dims1)
  ldr[ii] <- coef.names
  ldr <- c(character(length(hdrlines)),ldr,rownames(summaries))
  mtab <- cbind(ldr,mtab)
  mtab <- apply(mtab,1,paste,collapse=paste0(colsep))
  paste0(mtab,collapse=rowsep)
}


