mtable_format_print <- function(x,
          topsep="=",
          bottomsep="=",
          sectionsep="-",
          interaction.sep = " x ",
          center.at=getOption("OutDec"),
          align.integers=c("dot","right","left"),
          ...
          ){

  colsep <- " "
  rowsep <- "\n"
  
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
  
  align.integers <- match.arg(align.integers)
  coefs <- apply(coefs,2,centerAt,
                 at=center.at,
                 integers=align.integers)
  if(length(summaries)){
    summaries <- apply(summaries,2,centerAt,
                       at=center.at,
                       integers=align.integers)
    dim(summaries) <- dim(x$summaries)
    dimnames(summaries) <- dimnames(x$summaries)
  }
  
  col.vars <- rev(infos$col.vars)
  ans <- coefs
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
    ans <- format(ans,justify="centre")
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
    if(length(summaries)){
      if(i == 1) tmp <- c(tmp,rownames(summaries))
      else tmp <- c(tmp,rep("",nrow(summaries)))
    }
    tmp <- format(tmp,justify="left")
    leaders <- as.matrix(paste(leaders,tmp,colsep,sep=""))
  }
  ans <- paste(leaders,ans,sep=colsep)
  headlines <- seq(length(col.vars))
  if(x$kill.header){
    ans <- ans[-x$kill.header]
    headlines <- headlines[-x$kill.header]
  }
  coeflines <- (if(length(headlines)) max(headlines) else 0)+ seq(nrow(coefs))
  if(length(summaries))
    sumrylines <- max(coeflines) + seq(nrow(summaries))
  if((any(nchar(topsep)))){
    toprule <- rep(topsep,nchar(ans[1]))
    toprule <- paste(toprule,collapse="")
  } else
    toprule <- NULL
  if((any(nchar(sectionsep)))){
    secrule <- rep(sectionsep,nchar(ans[1]))
    secrule <- paste(secrule,collapse="")
  } else
    secrule <- NULL
  if((any(nchar(bottomsep)))){
    botrule <- rep(bottomsep,nchar(ans[1]))
    botrule <- paste(botrule,collapse="")
  } else
    botrule <- NULL
  ans <- c(
    toprule,
    if(length(headlines)) ans[headlines],
    if(length(headlines)) secrule,
    ans[coeflines],
    if(length(summaries)) secrule,
    if(length(summaries)) ans[sumrylines],
    botrule
  )
  ans <- paste0(paste(ans,collapse=rowsep),rowsep)
  return(ans)
}

