mtable_format_delim <- function(x,
                                colsep="\t",
                                rowsep="\n",
                                interaction.sep = " x ",
                                force.names = FALSE,
                                ...
){
  
  coefs <- x$coefficients
  summaries <- x$summaries
  
  num.models <- length(coefs)
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  max.coef.ldim <- max(coef.ldim)
  
  coef.dims1 <- unique(sapply(coef.dims,"[[",1))
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
  
  frmt1 <- function(name,coefs,summaries){
    
    coef.tab <- coefs
    dm <- dim(coefs)
    if(length(dm)==3) dm <- c(dm,1)
    dim(coef.tab) <- dm
    coef.tab <- aperm(coef.tab,c(1,3,2,4))
    dim(coef.tab) <- c(dm[1]*dm[3],dm[2]*dm[4])
    
    if(grp.coefs){
      hdr <- character(ncol(coef.tab))
      if(length(dim(coefs))>3){
        if(dm[4]>1)
          eq.names <- dimnames(coefs)[[4]]
        else
          eq.names <- ""

        ii <- seq(from=1,length=dm[4],by=dm[2])
        hdr[ii] <- eq.names
        }
      coef.tab <- rbind(hdr,coef.tab)
    }
    if(num.models>1 || force.names){
      hdr <- character(ncol(coef.tab))
      hdr[1] <- name
      coef.tab <- rbind(hdr,coef.tab)
    }
    if(length(summaries)){
      sum.tab <- matrix("",nrow=length(summaries),ncol=ncol(coef.tab))
      sum.tab[,1] <- summaries
      coef.tab <- rbind(coef.tab,sum.tab)
    }
    apply(coef.tab,1,paste,collapse=colsep)
  }
  
  if(length(x$model.groups)){
     for(i in seq_along(x$model.groups)){
      
      mg <- x$model.groups[[i]]
      mtab.m <- character()
      
      for(j in mg){
        mtab.m <- cbind(mtab.m,frmt1(names(coefs)[j],coefs[[j]],summaries[,j]))
      }
      model.name <- names(x$model.groups)[i]
      hdr <- rep("",ncol(mtab.m))
      hdr[1] <- model.name
      mtab.m <- rbind(hdr,mtab.m)
      mtab <- cbind(mtab,mtab.m)
    }
  }
  else {
    for(i in 1:length(coefs)){
      mtab <- cbind(mtab,frmt1(names(coefs)[i],coefs[[i]],summaries[,i]))
    }
  }
  
  smrylines <- seq(to=nrow(mtab),length=nrow(summaries))
  
  ldr <- character(length(coef.names)*coef.dims1)
  ii <- seq(from=1,length=length(coef.names),by=coef.dims1)
  ldr[ii] <- coef.names

  hldr <- NULL
  if(num.models>1 || force.names)
    hldr <- c(hldr,"")
  if(grp.coefs)
    hldr <- c(hldr,"")
  if(length(x$model.groups))
    hldr <- c("",hldr)
    
  ldr <- c(hldr,ldr,rownames(summaries))
  mtab <- cbind(ldr,mtab)
  mtab <- apply(mtab,1,paste,collapse=paste0(colsep))
  paste0(mtab,collapse=rowsep)
}


