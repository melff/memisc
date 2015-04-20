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
          center.summaries=FALSE,
          center.at=getOption("OutDec"),
          align.integers=c("dot","right","left"),
          ...
          ){

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
  col.vars <- rev(infos$col.vars)
  row.vars <- infos$row.vars[-x$kill.col]
  coefs <- apply(coefs,2,centerAt,
                 at=center.at,
                 integers=align.integers)
  coefs <- sub("(\\*+)","^{\\1}",coefs)
  coefs <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",coefs)
  if(!useDcolumn){
    tmpatt <- attributes(coefs)
    coefs <- paste("$",coefs,"$",sep="")
    attributes(coefs) <-tmpatt
  }
  
  if(length(summaries)){
    if(nrow(summaries)>1)
      summaries <- apply(summaries,2,centerAt,
                         at=center.at,
                         integers=align.integers)
    if(!useDcolumn){
      tmpatt <- attributes(summaries)
      summaries <- paste("$",summaries,"$",sep="")
      attributes(summaries) <-tmpatt
    }
    tmp.sumry <- array("",dim=c(nrow(summaries),ncol(coefs)/ncol(summaries),ncol(summaries)))
    if(center.summaries)
      sumpos <- (dim(tmp.sumry)[2]+1)%/%2
    else
      sumpos <- 1
    tmp.sumry[,sumpos,] <- summaries
    dim(tmp.sumry) <- c(nrow(summaries),ncol(coefs))
    ans <- rbind(coefs,tmp.sumry)
  }
  else ans <- coefs
  
  header <- character(length(col.vars))
  
  for(i in 1:length(col.vars)){
    tmp.header <- character(NCOL(ans))
    cv <- col.vars[[i]]
    lcv <- length(cv)
    tmp.header[] <- cv
    mcols <- ncol(coefs)/length(tmp.header)
    tmp.header <- paste("\\multicolumn{",mcols,"}{c}{",trimws(tmp.header),"}",sep="")
    if(i == length(col.vars) && length(col.vars) > 1)
      tmp.header <- paste(tmp.header,collapse=" && ")
    else
      tmp.header <- paste(tmp.header,collapse=" & ")
    if(length(col.vars)>1)
      tmp.header <- c(rep("",length(row.vars)+1),t(tmp.header))
    else
      tmp.header <- c(rep("",length(row.vars)),t(tmp.header))
    tmp.header <- paste(tmp.header,collapse="&")
    header[i] <- tmp.header
    
    ans <- format(ans,justify="centre")
    dim(ans) <- c(nrow(ans),lcv,ncol(ans)/lcv)
    if(i == length(col.vars) && length(col.vars) > 1)
      ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=" && ")))
    else
      ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=" & ")))
  }
  leaders <- character(NROW(coefs)+if(length(summaries)) nrow(summaries) else 0)
  for(i in 1:length(row.vars)){
    tmp <- matrix("",nrow=length(row.vars[[i]]),
                  ncol=nrow(coefs)/length(row.vars[[i]]))
    tmp[,1] <- row.vars[[i]]
    tmp <- c(t(tmp))
    if(length(summaries)){
      if(i == 1) tmp <- c(tmp,rownames(summaries))
      else tmp <- c(tmp,rep("",nrow(summaries)))
    }
    tmp <- format(tmp,justify="left")
    if(i < length(row.vars) || length(col.vars) > 1)
      leaders <- as.matrix(paste(leaders,tmp," & " ,sep=""))
    else
      leaders <- as.matrix(paste(leaders,tmp,sep=""))
  }
  ans <- paste(leaders,ans,sep=" & ")
  header <- paste(header,"\\\\",sep="")
  
  if(length(cmidrule) && length(col.vars)>1){
    ccol.vars <- rev(col.vars[-1])
    n.grps <- length(ccol.vars[[1]])
    len.grps <- ncol(coefs)/n.grps + 1
    strt.grps <- length(row.vars)+1 + (seq(n.grps)-1)*len.grps+1
    cmidrules <- character(length(header))
    for(i in 1:length(ccol.vars)){
      n.cmrl <- length(ccol.vars[[i]])
      len.cmrl <- ncol(coefs)/n.cmrl
      per.grp <- n.cmrl/n.grps
      strt.igrp <- (seq(per.grp)-1)*len.cmrl
      end.igrp <- seq(per.grp)*len.cmrl - 1
      strt.cmrl <- c(outer(strt.igrp,strt.grps,"+"))
      end.cmrl <- c(outer(end.igrp,strt.grps,"+"))
      ccmidrule <- paste(cmidrule,"{",strt.cmrl,"-",end.cmrl,"}",sep="")
      cmidrules[i] <- paste(ccmidrule,collapse="")
    }
    header <- cbind(rev(header),rev(cmidrules))
    if(x$kill.header)
      header <- header[-x$kill.header,,drop=FALSE]
    header <- c(t(header))
    header <- header[-length(header)]
  }
  else{
    header <- rev(header)
    if(x$kill.header)
      header <- header[-x$kill.header]
  }
  ans <- paste(ans,"\\\\",sep="")
  coeflines <- seq(nrow(coefs))
  if(length(summaries))
    sumrylines <- max(coeflines) + seq(nrow(summaries))
  ans <- c(
    toprule,
    if(length(header))header,
    if(length(header))midrule,
    ans[coeflines],
    if(length(summaries)) midrule,
    if(length(summaries)) ans[sumrylines],
    bottomrule
  )
  
  leader.spec <- paste(rep("l",length(row.vars)),collapse="")
  
  if(length(col.vars) > 1){
    
    sz.eq <- ncol(coefs)/length(rev(col.vars)[[1]])
    n.eq <- length(rev(col.vars)[[1]])
    coef.spec <- matrix("",nrow=n.eq,ncol=sz.eq)
    coef.spec[] <- colspec
    coef.spec <- apply(coef.spec,1,paste,collapse="")
  }
  else {
    coef.spec <- character(ncol(coefs))
    coef.spec[] <- colspec
  }
  
  tabspec <- c(leader.spec,coef.spec)
  if(length(col.vars) > 1)
    tabspec <- paste(tabspec,collapse="c")
  else
    tabspec <- paste(tabspec,collapse="")
  tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
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




