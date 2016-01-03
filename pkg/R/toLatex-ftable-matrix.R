.last <- function(x) x[[length(x)]]
.getElts <- function(x,i) lapply(x,"[[",i)

toLatex.ftable_matrix <- function(object,
                                  show.titles=TRUE,
                                  digits=0,
                                  format="f",
                                  useDcolumn=TRUE,
                                  colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
                                  LaTeXdec=".",
                                  ddigits=digits,
                                  useBooktabs=TRUE,
                                  toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
                                  midrule=if(useBooktabs) "\\midrule" else "\\hline",
                                  cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
                                  bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
                                  compact=FALSE,
                                  varontop,varinfront,
                                  groupsep="3pt",
                                  grouprule=midrule,
                                  ...)
{
  
  row.vars <- attr(object,"row.vars")
  col.vars <- attr(object,"col.vars")
  
  N <- nrow(object)
  M <- ncol(object)
  
  n <- sapply(object[,1],nrow)
  m <- sapply(object[1,],ncol)
  n.row.vars <- sapply(row.vars,length)
  n.col.vars <- sapply(col.vars,length)
  max.n.row.vars <- max(n.row.vars)
  max.n.col.vars <- max(n.col.vars)
  
  if(missing(varontop)) varontop <- max(n.col.vars) <= 1
  if(missing(varinfront)) varinfront <- max(n.row.vars) <= 1
  
  d <- digits
  digits <- integer(M)
  digits[] <- d
  
  fo <- format
  format <- integer(M)
  format[] <- fo
  
  csp <- colspec
  colspec <- character(length(m))
  colspec[] <- csp
  
  cv.desc <- sapply(col.vars,cv_get_desc,compact=compact)
  start.g <- cv.desc["start.g",]
  end.g <- cv.desc["end.g",]
  nms.cv <- cv.desc["nms.cv",]
  mcols <- cv.desc["mcols",]
  width <- cv.desc["width",]
  ii <- cv.desc["ii",]
  cv.repg <- cv.desc["cv.repg",]
  
  headers <- mapply(ltfm_mkHeader,col.vars,n.col.vars,mcols,width,cv.repg,ii,varontop,compact,SIMPLIFY=FALSE)
  
  l.hd <- sapply(headers,length)
  max.l.hd <- max(l.hd)
  
  for(i in 1:M){
    
    jj <- seq.int(to=max.l.hd,length.out=l.hd[i])
    tmp.hdr1 <- paste0(rep("",width[[i]]),collapse="&")
    if(n.col.vars[i]>1||!varontop & any(nzchar(nms.cv[[i]])) )
      tmp.hdr1 <- paste0("&",tmp.hdr1)
    
    tmp.header <- vector("list",max.l.hd)
    tmp.header[] <- list(tmp.hdr1)
    tmp.header[jj] <- headers[[i]]
    headers[[i]] <- tmp.header
  }
  
  headers <- lapply(1:max.l.hd,.getElts,x=headers)
  headers <- lapply(headers,unlist)
  
  for(i in 1:M){
    
    if(i==1){
      start.g[[i]] <- mapply(`+`,start.g[[i]],max.n.row.vars,SIMPLIFY=FALSE)
      end.g[[i]] <- mapply(`+`,end.g[[i]],max.n.row.vars,SIMPLIFY=FALSE)
    }
    else if(compact){
      start.g[[i]] <- mapply(`+`,start.g[[i]],tmp.end.g,SIMPLIFY=FALSE)
      end.g[[i]] <- mapply(`+`,end.g[[i]],tmp.end.g,SIMPLIFY=FALSE)
    }
    else {
      start.g[[i]] <- mapply(`+`,start.g[[i]],tmp.end.g+1,SIMPLIFY=FALSE)
      end.g[[i]] <- mapply(`+`,end.g[[i]],tmp.end.g+1,SIMPLIFY=FALSE)
    }
    
    if(n.col.vars[[i]] > 1 | (!varontop & nzchar(nms.cv[[i]]))[1]){
      start.g[[i]] <- mapply(`+`,start.g[[i]],1,SIMPLIFY=FALSE)
      end.g[[i]] <- mapply(`+`,end.g[[i]],1,SIMPLIFY=FALSE)
    }
    tmp.end.g <- .last(end.g[[i]])
    tmp.end.g <- .last(tmp.end.g)
  }
  
  for(i in 1:M){
    
    jj <- seq.int(to=max.n.col.vars,length.out=n.col.vars[i])
    
    tmp.start.g <- vector("list",max.n.col.vars)
    if(varontop || n.col.vars[i]>1)
      tmp.start.g[jj] <- start.g[[i]]
    start.g[[i]] <- tmp.start.g
    tmp.end.g <- vector("list",max.n.col.vars)
    if(varontop || n.col.vars[i]>1)
      tmp.end.g[jj] <- end.g[[i]]
    end.g[[i]] <- tmp.end.g
  }
  
  start.g <- lapply(1:max.n.col.vars,.getElts,x=start.g)
  end.g <- lapply(1:max.n.col.vars,.getElts,x=end.g)
  start.g <- lapply(start.g,unlist)
  end.g <- lapply(end.g,unlist)
  
  if(varinfront) {
    
    start.g <- mapply(`+`,start.g,1,SIMPLIFY=FALSE)
    end.g <- mapply(`+`,end.g,1,SIMPLIFY=FALSE)
  }
  
  header <- unlist(lapply(headers,paste0,collapse=if(compact)"&" else "&&"))
  len.hdr <- length(header)
  lheader <- matrix("",ncol=max.n.row.vars,nrow=len.hdr)
  lheader[len.hdr,seq.int(to=max.n.row.vars,length.out=n.row.vars[1])] <- names(row.vars[[1]])
  if(varinfront) lheader[,1] <- ""
  lheader <- apply(lheader,1,paste0,collapse="&")
  header <- paste(lheader,header,sep="&")
  if(varinfront)
    header <- paste0("&",header)
  
  cln <- mapply(mkclines,cmidrule,start.g,end.g)
  cln <- sapply(cln,paste0,collapse="")
  
  header <- paste0(header,"\\\\")
  
  if(varontop)
    header[-1] <- paste0(cln,"\n",header[-1])
  else
    header[-1] <- paste0(cln[-1],"\n",header[-1])
  
  leaders <- mapply(ltfm_mkLeader,row.vars,n.row.vars,n,SIMPLIFY=FALSE)
  
  if(varinfront){
    
    for(i in 1:N){
      
      nm.row.vars.i <- names(row.vars[[i]])
      tmp.leaders <- matrix("",nrow=n[i],ncol=max.n.row.vars+1)
      jj <- seq.int(to=max.n.row.vars+1,length.out=n.row.vars[i])
      tmp.leaders[,jj] <- leaders[[i]]
      if(nzchar(nm.row.vars.i[1]))
        tmp.leaders[1,1] <- nm.row.vars.i[1]
      else if(varinfront && length(n[i] < 2)){
        tmp.leaders[1,1] <- tmp.leaders[1,max.n.row.vars+1]
        tmp.leaders[1,max.n.row.vars+1] <- ""
      }
      leaders[[i]] <- tmp.leaders
      if(i > 1 && n.row.vars[i] > 1 && any(nzchar(nm.row.vars.i[-n.row.vars[i]]))){
        
        tmp.leaders <- character(max.n.row.vars)
        tmp.leaders[jj] <- names(row.vars[[i]])
        leaders[[i]] <- rbind(tmp.leaders,leaders[[i]])
      }
    }
  }
  else {
    
    for(i in 1:N){
      
      tmp.leaders <- matrix("",nrow=n[i],ncol=max.n.row.vars)
      jj <- seq.int(to=max.n.row.vars,length.out=n.row.vars[i])
      tmp.leaders[,jj] <- leaders[[i]]
      leaders[[i]] <- tmp.leaders
      if(i > 1 && any(nzchar(names(row.vars[[i]])))
      ){
        tmp.leaders <- character(max.n.row.vars)
        tmp.leaders[jj] <- names(row.vars[[i]])
        leaders[[i]] <- rbind(tmp.leaders,leaders[[i]])
      }
    }
  }
  
  body <- array(list(),dim=dim(object))
  
  for(j in 1:M){
    for(i in 1:N){
      
      tmp.bdy <- ltfm_mkBody(object[[i,j]],ii[[j]],format[[j]],digits[[j]])
      if(n.col.vars[[j]] > 1 |  (!varontop & nzchar(nms.cv[[j]]))[1])
        tmp.bdy <- cbind("",tmp.bdy)
      if(j > 1 && !compact)
        tmp.bdy <- cbind("",tmp.bdy)
      if(varinfront){
        
        nm.row.vars.i <- names(row.vars[[i]])
        if(i > 1 && n.row.vars[i] > 1 && any(nzchar(nm.row.vars.i[-n.row.vars[i]])))
          tmp.bdy <- rbind("",tmp.bdy)
      }
      else{
        
        if(i > 1 && any(nzchar(names(row.vars[[i]]))))
          tmp.bdy <- rbind("",tmp.bdy)
      }
      body[[i,j]] <- tmp.bdy
    }
  }
  
  ans <- list()
  for(i in 1:N){
    
    tmp.bdy <- cbind(leaders[[i]],do.call(cbind,body[i,,drop=FALSE]))
    tmp.bdy <- apply(tmp.bdy,1,paste0,collapse="&")
    tmp.rowsep <- rep("\\\\",length(tmp.bdy))
    if(i<N && (!varinfront || max.n.row.vars > 1) && !compact)
      tmp.rowsep[length(tmp.bdy)] <- paste0(tmp.rowsep[length(tmp.bdy)],"[",groupsep,"]")
    if(i>1 && (!varinfront || max.n.row.vars > 1) && any(nzchar(names(row.vars[[i]])))){
      
      tmp.rowsep[1] <- paste0(tmp.rowsep[1],"\n",grouprule)
    }
    tmp.bdy <- paste0(tmp.bdy,tmp.rowsep)
    if(i > 1 && varinfront && max.n.row.vars == 1)
      tmp.bdy[1] <- paste(grouprule,tmp.bdy[1])
    ans[[i]] <- tmp.bdy
  }
  ans <- unlist(ans)
  
  ans <- c(toprule,header,midrule,ans,bottomrule)
  
  tabspec <- mapply(ltfm_mkTabspec,colspec,ii,m,USE.NAMES=FALSE)
  
  for(i in 1:M){
    
    if(n.col.vars[i] > 1 | (!varontop & nzchar(nms.cv[[i]]))[1])
      tabspec[i] <- paste0("l",tabspec[i])
    if(i>1 & !compact)
      tabspec[i] <- paste0("c",tabspec[i])
  }
  tabspec <- c(rep("l",max.n.row.vars),tabspec)
  if(varinfront) tabspec <- c("l",tabspec)
  tabspec <- paste0(tabspec,collapse="")
  
  tabbegin <- paste0("\\begin{tabular}{",tabspec,"}")
  tabend <- "\\end{tabular}"
  ans <- c(tabbegin,ans,tabend)
  
  structure(ans,class="Latex")
}

cv_get_desc <- function(col.vars,compact){
  
  m <- prod(sapply(col.vars,length))
  n.col.vars <- length(col.vars)
  nms.cv <- names(col.vars)
  l.cv <- sapply(col.vars,length)
  cv.sizeg <- rev(cumprod(rev(l.cv)))
  cv.repg <- m/cv.sizeg
  
  if(compact){
    cv.stepg <- m/cv.repg
    width <- m
  }
  else {
    
    width <- cv.repg[n.col.vars]*(cv.sizeg[n.col.vars]+1)
    cv.stepg <- width/cv.repg
    width <- width - 1
  }
  
  mcols <- c(if(compact) cv.sizeg else cv.stepg-1,1)

  if(compact){
    start.g <- lapply(1:n.col.vars,function(i){
      seq(from=1,length=cv.repg[i],by=cv.stepg[i])
    })
    end.g <- mapply(`+`,start.g,cv.stepg-1)
  }
  else{
    start.g <- lapply(1:n.col.vars,function(i){
      seq(from=1,length=cv.repg[i],by=cv.stepg[i])
    })
    end.g <- mapply(`+`,start.g,cv.stepg-2)
  }
  start.g.n <- start.g[[n.col.vars]]
  end.g.n <- end.g[[n.col.vars]]
  ii <- unlist(mapply(seq,from=start.g.n,to=end.g.n,SIMPLIFY=FALSE))
  
  list(
    nms.cv=nms.cv,
    l.cv=l.cv,
    cv.sizeg=cv.sizeg,
    cv.repg=cv.repg,
    cv.stepg=cv.stepg,
    width=width,
    mcols=mcols,
    start.g=start.g,
    end.g=end.g,
    start.g.n=start.g.n,
    end.g.n=end.g.n,
    ii=ii
  )
}

ltfm_mkHeader <- function(col.vars,n.col.vars,mcols,width,cv.repg,ii,varontop,compact){
  
  nms.cv <- names(col.vars)
  
  header <- character()
  for(i in 1:n.col.vars){
    
    if(i==1 && varontop){
      if(nzchar(nms.cv[i]))
        tmp.header <- paste0("\\multicolumn{",mcols[i],"}{c}{",nms.cv[i],"}")
      else
        tmp.header <- ""
      if(n.col.vars>1)
        tmp.header <- paste0("&",tmp.header)
      header <- append(header,list(tmp.header))
    }
    
    if(i==n.col.vars){
      tmp.header <- character(width)
      tmp.header[ii] <- paste0("\\multicolumn{",mcols[i+1],"}{c}{",rep(col.vars[[i]],cv.repg[i]),"}")
      tmp.header <- paste0(tmp.header,collapse="&")
    }
    else {
      tmp.header <- paste0("\\multicolumn{",mcols[i+1],"}{c}{",rep(col.vars[[i]],cv.repg[i]),"}")
      tmp.header <- paste0(tmp.header,collapse=if(compact)"&"else"&&")
    }
    if(n.col.vars==1){
      
      if(nzchar(nms.cv[i]) && !varontop)
        tmp.header <- paste0(nms.cv[i],":&",tmp.header)
      
    }
    else {
      
      if((i > 1 || !varontop) && nzchar(nms.cv[i]))
        tmp.header <- paste0(nms.cv[i],":&",tmp.header)
      else
        tmp.header <- paste0("&",tmp.header)
    }
    
    header <- append(header,list(tmp.header))
  }
  header
}

mkclines <- function(cmidrule,start.g,end.g){
  
  len <- end.g - start.g + 1
  start.g <- start.g[len>1]
  end.g <- end.g[len>1]
  paste0(cmidrule,"{",start.g,"-",end.g,"}")
}

ltfm_mkLeader <- function(row.vars,n.row.vars,n) {
  
  leader <- matrix("",nrow=n,ncol=n.row.vars)
  
  for(i in 1:n.row.vars){
    
    rv <- row.vars[[i]]
    rep.rv <- if(i==1)1 else nn.rv
    rv <- rep(rv,rep.rv)
    nn.rv <- length(rv)
    rv.n <- n/nn.rv
    pos <- (1:nn.rv)*rv.n
    pos <- pos - rv.n + 1
    leader[pos,i] <- rv
  }
  leader
}

ltfm_mkBody <- function(object,ii,format,digits){
  
  n <- nrow(object)
  m <- ncol(object)
  
  d <- digits
  digits <- integer(m)
  digits[] <- d
  
  fo <- format
  format <- integer(m)
  format[] <- fo
  
  total.width <- max(ii)
  
  body <- array("",dim=c(nrow(object),total.width))
  
  for(i in seq(along=digits))
    body[,ii[i]] <- formatC(object[,i],digits=digits[i],format=format[i])
  
  sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
}

ltfm_mkTabspec <- function(colspec,ii,m){
  
  csp <- character(m)
  csp[] <- colspec
  colspec <- csp
  
  total.width <- length(ii)
  tabspec <-rep("c",total.width)
  tabspec[ii] <- colspec
  paste(tabspec,collapse="")
}


