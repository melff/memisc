toLatex.ftable <- function(object,
          show.titles=TRUE,
          digits=0,
          format="f",
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=digits,
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline\n",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          extrarowsep = NULL,
          ...){
  row.vars <- attr(object,"row.vars")
  col.vars <- attr(object,"col.vars")
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n <- nrow(object)
  m <- ncol(object)
  d <- digits
  digits <- integer(m)
  digits[] <- d
  #print(digits)
  fo <- format
  format <- integer(m)
  format[] <- fo
  body <- array("",dim=dim(object))
  for(i in seq(along=digits)) {
    #print(digits[i])
    body[,i] <- formatC(object[,i],digits=digits[i],format=format[i])
    }
  body <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
  #str(digits)
  #str(body)
  header <- character(n.col.vars)
  clines <- character(n.col.vars)
  ncols <- ncol(body)
  n.col.grps <- length(col.vars[[1]])
  header <- character(n.col.vars)
  clns <- character(n.col.vars)
  c.starts <- list()
  c.ends <- list()
  mcols <- integer(n.col.vars)
  ng <- integer(n.col.vars)
  mcols <- integer(n.col.vars)
  for(i in n.col.vars:1){
    if(i == n.col.vars){
      mcols[i] <- 1
      ng[i] <- ncols
    }
    else {
      mcols[i] <- length(col.vars[[i+1]])*mcols[i+1]
      ng[i] <- ncols/mcols[i]
    }
  }
  for(i in 1:n.col.vars){
    
    cv <- col.vars[[i]]
    lcv <- length(cv)
    tmp.header <- character(ng[i])
    tmp.header[] <- cv
    tmp.header <- paste("\\multicolumn{",mcols[i],"}{c}{",tmp.header,"}",sep="")
    if(i == n.col.vars && n.col.vars > 1){
      dim(tmp.header) <- c(mcols[i-1],ng[i-1])
      tmp.header <- apply(tmp.header,2,paste,collapse="&")
      header[i] <- paste(tmp.header,collapse="&&")
      }
    else if (n.col.vars > 1)
      header[i] <- paste(tmp.header,collapse="&&")
    else
      header[i] <- paste(tmp.header,collapse="&")
    if(length(cmidrule)){
      if(i == n.col.vars)
        c.starts[[i]] <- length(row.vars) + 2 + (seq(lcv) - 1)*mcols[i]
      else
        c.starts[[i]] <- length(row.vars) + 2 + (seq(lcv) - 1)*(mcols[i]+1)
      c.ends[[i]] <- c.starts[[i]] + (mcols[i]-1)
      tmp.cln <- paste(cmidrule,"{",c.starts[[i]],"-",c.ends[[i]],"}",sep="")
      clns[i] <- paste(tmp.cln,collapse="")
    }
  }
  for(i in 1:n.col.vars){
    if(i == n.col.vars)
      header[i] <- paste(header[i],collapse=" & ")
    else
      header[i] <- paste(header[i],collapse=" && ")
  }
  
  if(show.titles){
    if(length(names(col.vars))==1){
      
      hleaders <- matrix("",nrow=2,ncol=n.row.vars)
      hleaders[2,] <- names(row.vars)
      hleaders <- apply(hleaders,1,paste,collapse="&")
      
      header <- c(paste("\\multicolumn{",m,"}{c}{",names(col.vars),"}",sep=""),
                  header)
    }
    else {
      hleaders <- matrix("",nrow=n.col.vars,ncol=n.row.vars+1)
      hleaders[,n.row.vars+1] <- paste0(names(col.vars),":")
      hleaders[n.col.vars,1:n.row.vars] <- names(row.vars)
      hleaders <- apply(hleaders,1,paste,collapse="&")
    }
  } else {
    hleaders <- matrix("",nrow=n.col.vars,ncol=n.row.vars)
    hleaders <- apply(hleaders,1,paste,collapse="&")
  }
  
  leaders <- matrix("",nrow=nrow(body),ncol=n.row.vars)
  lrv <- integer(n.row.vars)
  for(i in 1:n.row.vars){
    rv <- row.vars[[i]]
    if(i == 1)
      lrv[i] <- length(rv)
    else
      lrv[i] <- length(rv) * lrv[i-1]
    tmp.leaders <- matrix("",ncol=lrv[i],nrow=n/lrv[i])
    tmp.leaders[1,] <- rv
    leaders[,i] <- c(tmp.leaders)
  }

  leaders <- format(leaders)
  leaders <- apply(leaders,1,paste,collapse="&")
  
  lcv <- length(col.vars[[n.col.vars]])
  dim(body) <- c(n,lcv,m/lcv)
  body <- format(body)
  body <- apply(body,c(1,3),paste,collapse=" & ")
  body <- apply(body,1,paste,collapse=" && ")

  if(show.titles && length(names(col.vars))>1)
    header <- paste(hleaders,header,sep=" & ")
  else
    header <- paste(hleaders,header,sep=" && ")
  
  header <- paste(header,"\\\\",sep="")
  if(length(cmidrule)){
    header <- c(rbind(header,clns))
    header <- header[-length(header)]
  }
  body <- paste(leaders,body,sep=" && ")
  
  if(!length(extrarowsep))
    body <- paste(body,"\\\\",sep="")
  else {
    rowsep <- rep("\\\\",NROW(body))
    .extrarowsep <- rep("",NROW(body))
    lrv <- length(row.vars[[n.row.vars]])
    ii <- seq(NROW(body)%/%lrv)*lrv
    if(show.titles && length(names(row.vars))) ii <- ii+1
    .extrarowsep[ii] <- paste("[",extrarowsep,"]",sep="")
    rowsep <- paste(rowsep,.extrarowsep,sep="")
    body <- paste(body,rowsep,sep="")
  }

  ans <- c(toprule,header,midrule,body,bottomrule)
  
  leader.spec <- paste(rep("l",length(row.vars)+1),collapse="")
  body.spec <- character(m)
  body.spec[] <- colspec
  i <- rep(seq_len(m/lcv),each=lcv)
  body.spec <- split(body.spec,i)
  body.spec <- sapply(body.spec,paste,collapse="")
  body.spec <- paste(body.spec,collapse="c")
  tabspec <- paste(leader.spec,body.spec,sep="")
  
  tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
  tabend <- "\\end{tabular}"
  ans <- c(tabbegin,ans,tabend)
  structure(ans,class="Latex")
}