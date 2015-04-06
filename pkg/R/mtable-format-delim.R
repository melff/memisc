mtable_format_delim <- function(x,
          colsep="\t",
          rowsep="\n",
          interaction.sep = " x ",
          ...
          ){

  coldims <- dim(x$coefficients)[x$as.col]
  nhrows <- length(coldims)

  coefnames <- dimnames(x$coefficients)[[x$coef.dim]]
  if(interaction.sep !=" x ")
    coefnames <- gsub(" x ",interaction.sep,coefnames,fixed=TRUE)
  dimnames(x$coefficients)[[x$coef.dim]] <- coefnames
  coefs <- ftable(as.table(x$coefficients),row.vars=rev(x$as.row),
    col.vars=rev(x$as.col)
    )
  infos <- attributes(coefs)
  summaries <- x$summaries
  
  ans <- trimws(coefs)
  col.vars <- rev(infos$col.vars)
  
  for(i in 1:length(col.vars)){
    header <- character(NCOL(ans))
    cv <- col.vars[[i]]
    lcv <- length(cv)
    header[] <- cv
    ans <- rbind(header,ans)
    if(length(summaries)){
      if(i == length(col.vars)){
        if(ncol(ans)>ncol(summaries)){
          tmp <- summaries
          summaries <- matrix("",nrow=nrow(tmp),ncol=ncol(ans))
          summaries[,1] <- tmp
          rownames(summaries) <- rownames(tmp)
        }
        ans <- rbind(ans,summaries)
      }
    }
    dim(ans) <- c(nrow(ans),lcv,ncol(ans)/lcv)
    ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=colsep)))
  }
  row.vars <- infos$row.vars[-x$kill.col]
  leaders <- character(NROW(ans))
  for(i in 1:length(row.vars)){
    tmp <- matrix("",nrow=length(row.vars[[i]]),
                  ncol=nrow(coefs)/length(row.vars[[i]]))
    tmp[,1] <- row.vars[[i]]
    tmp <- c(rep("",length(col.vars)),t(tmp))
    if(i == 1) tmp <- c(tmp,rownames(summaries))
    else tmp <- c(tmp,rep("",nrow(summaries)))
    if(i == length(row.vars))
      leaders <- as.matrix(paste(leaders,tmp,sep=""))
    else
      leaders <- as.matrix(paste(leaders,tmp,colsep,sep=""))
  }
  ans <- paste(leaders,ans,sep=colsep)
  ans <- ans[-x$kill.header]
  ans <- paste(ans,rowsep,sep="")
  return(paste(ans,collapse=""))
}


