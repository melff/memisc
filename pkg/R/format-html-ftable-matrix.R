format_html.ftable_matrix <- function(x,
                               show.titles=TRUE,
                               digits=0,
                               format="f",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               varontop,varinfront,
                               ...){
  
  style <- ftable_format_stdstyle
  first.col <- c("padding-left"="0.3em")
  toprule <- c("border-top"=paste0(midrule,"px solid"))
  bottomrule <- c("border-bottom"=paste0(midrule,"px solid"))
  midrule_above <- c("border-top"=paste0(midrule,"px solid"))
  midrule <- c("border-bottom"=paste0(midrule,"px solid"))
  align.right <- c("text-align"="right")  
  align.left <- c("text-align"="left")  
  align.center <- c("text-align"="center")
  lrpad <- c("padding-left"="0.3em","padding-right"="0.3em")
  
  row.vars <- attr(x,"row.vars")
  col.vars <- attr(x,"col.vars")

  N <- nrow(x)
  M <- ncol(x)
  
  n <- sapply(x[,1],nrow)
  m <- sapply(x[1,],ncol)
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
  
  # cv_get_desc is defined in toLatex-ftable-matrix.R
  cv.desc <- sapply(col.vars,cv_get_desc,compact=TRUE)
  end.g <- cv.desc["end.g",]
  nms.cv <- cv.desc["nms.cv",]
  mcols <- cv.desc["mcols",]
  width <- cv.desc["width",]
  ii <- cv.desc["ii",]
  cv.repg <- cv.desc["cv.repg",]
  
  if(split.dec) mcols <- lapply(mcols,`*`,3)
  
  headers <-mapply(htfm_mkHeader,col.vars,n.col.vars,mcols,width,cv.repg,ii,varontop,SIMPLIFY=FALSE)

  
  l.hd <- sapply(headers,length)
  max.l.hd <- max(l.hd)
  
  for(i in 1:M){
    
    jj <- seq.int(to=max.l.hd,length.out=l.hd[i])
    if(split.dec)
      tmp.hdr1 <- mk_td(rep("",width[[i]]),extra="colspan=\"3\"")
    else
      tmp.hdr1 <- mk_td(rep("",width[[i]]))
    if(n.col.vars[i]>1||!varontop & any(nzchar(nms.cv[[i]])) )
      tmp.hdr1 <- c(mk_td(""),tmp.hdr1)
    tmp.header <- vector("list",max.l.hd)
    tmp.header[] <- list(tmp.hdr1)
    tmp.header[jj] <- headers[[i]]
    headers[[i]] <- tmp.header
  }
  
  headers <- lapply(1:max.l.hd,.getElts,x=headers)
  headers <- lapply(headers,unlist)
  
  header <- unlist(lapply(headers,paste0,collapse=""))
  len.hdr <- length(header)
  lheader <- matrix("",ncol=max.n.row.vars,nrow=len.hdr)
  lheader[len.hdr,seq.int(to=max.n.row.vars,length.out=n.row.vars[1])] <- names(row.vars[[1]])
  if(varinfront) lheader[,1] <- ""
  lheader[] <- mk_td(lheader) 
  lheader <- apply(lheader,1,paste0,collapse="")
  header <- paste0(lheader,header)
  if(varinfront)
    header <- paste0(mk_td(""),header)
  
  leaders <- mapply(htfm_mkLeader,row.vars,n.row.vars,n,SIMPLIFY=FALSE)
  
  if(varinfront){
    
    for(i in 1:N){
      
      nm.row.vars.i <- names(row.vars[[i]])
      tmp.leaders <- mk_td("")
      tmp.leaders <- matrix(tmp.leaders,nrow=n[i],ncol=max.n.row.vars+1)
      jj <- seq.int(to=max.n.row.vars+1,length.out=n.row.vars[i])
      tmp.leaders[,jj] <- leaders[[i]]
      if(nzchar(nm.row.vars.i[1]))
        tmp.leaders[1,1] <- mk_td(nm.row.vars.i[1])
      else if(varinfront && length(n[i] < 2)){
        tmp.leaders[1,1] <- tmp.leaders[1,max.n.row.vars+1]
        tmp.leaders[1,max.n.row.vars+1] <- mk_td("")
      }
      leaders[[i]] <- tmp.leaders
      if(i > 1 && n.row.vars[i] > 1 && any(nzchar(nm.row.vars.i[-n.row.vars[i]]))){
        
        tmp.leaders <- character(max.n.row.vars)
        tmp.leaders[jj] <- mk_td(names(row.vars[[i]]))
        leaders[[i]] <- rbind(tmp.leaders,leaders[[i]])
      }
    }
  }
  else {
    
    for(i in 1:N){
      
      tmp.leaders <- matrix(mk_td(""),nrow=n[i],ncol=max.n.row.vars)
      jj <- seq.int(to=max.n.row.vars,length.out=n.row.vars[i])
      tmp.leaders[,jj] <- leaders[[i]]
      leaders[[i]] <- tmp.leaders
      if(i > 1 && any(nzchar(names(row.vars[[i]])))
      ){
        tmp.leaders <- character(max.n.row.vars)
        tmp.leaders[jj] <- mk_td(names(row.vars[[i]]))
        leaders[[i]] <- rbind(tmp.leaders,leaders[[i]])
      }
    }
  }
  
  body <- array(list(),dim=dim(x))
  
  for(j in 1:M){
    for(i in 1:N){
      
      tmp.bdy <- htfm_mkBody(x[[i,j]],ii[[j]],format[[j]],digits[[j]],split.dec)
      if(n.col.vars[[j]] > 1 |  (!varontop & nzchar(nms.cv[[j]]))[1])
        tmp.bdy <- cbind(mk_td(""),tmp.bdy)
      if(varinfront){
        
        nm.row.vars.i <- names(row.vars[[i]])
        if(i > 1 && n.row.vars[i] > 1 && any(nzchar(nm.row.vars.i[-n.row.vars[i]])))
          tmp.bdy <- rbind(mk_td(""),tmp.bdy)
      }
      else{
        
        if(i > 1 && any(nzchar(names(row.vars[[i]]))))
          tmp.bdy <- rbind(mk_td(""),tmp.bdy)
      }
      body[[i,j]] <- tmp.bdy
    }
  }
  
  ans <- list()
  for(i in 1:N){
    
    tmp.bdy <- cbind(leaders[[i]],do.call(cbind,body[i,,drop=FALSE]))
    tmp.bdy <- apply(tmp.bdy,1,paste0,collapse="")
    ans[[i]] <- tmp.bdy
  }
  ans <- c(header,unlist(ans))
  ans <- mk_tr(ans)
  ans <- c("<table class=\"ftable\" style=\"border-collapse: collapse;\">",
           ans,
           "</table>")
  
  ans <- paste0(ans,collapse="\n")
  return(ans)
  
  
}


htfm_mkHeader <- function(col.vars,n.col.vars,mcols,width,cv.repg,ii,varontop){
  
  nms.cv <- names(col.vars)
  
  header <- character()
  for(i in 1:n.col.vars){
    
    if(i==1 && varontop){
      if(nzchar(nms.cv[i])){
        tmp.header <- mk_td(nms.cv[i],extra=paste0("colspan=\"",mcols[i],"\""))
        if(n.col.vars>1)
          tmp.header <- c(mk_td(""),tmp.header)
      }
      else
        tmp.header <- ""
      tmp.header <- paste0(tmp.header,collapse="")
      header <- append(header,list(tmp.header))
    }
    
    if(i==n.col.vars){
      tmp.header <- character(width)
      tmp.header[ii] <- mk_td(rep(col.vars[[i]],cv.repg[i]),
                              extra=paste0("colspan=\"",mcols[i+1],"\""))
    }
    else {
      tmp.header <- mk_td(rep(col.vars[[i]],cv.repg[i]),
                          extra=paste0("colspan=\"",mcols[i+1],"\""))
    }
    if(n.col.vars==1){
      
      if(nzchar(nms.cv[i]) && !varontop)
        tmp.header <- c(mk_td(paste0(nms.cv[i],":")),tmp.header)
      
    }
    else {
      
      if((i > 1 || !varontop) && nzchar(nms.cv[i]))
        tmp.header <- c(mk_td(paste0(nms.cv[i],":")),tmp.header)
      else
        tmp.header <- c(mk_td(""),tmp.header)
    }
    header <- append(header,list(tmp.header))
  }
  header
}

htfm_mkLeader <- function(row.vars,n.row.vars,n) {
  
  leader <- matrix(mk_td(""),nrow=n,ncol=n.row.vars)
  
  for(i in 1:n.row.vars){
    
    rv <- row.vars[[i]]
    rep.rv <- if(i==1)1 else nn.rv
    rv <- rep(rv,rep.rv)
    nn.rv <- length(rv)
    rv.n <- n/nn.rv
    pos <- (1:nn.rv)*rv.n
    pos <- pos - rv.n + 1
    leader[pos,i] <- mk_td(rv)
  }
  leader
}

htfm_mkBody <- function(x,ii,format,digits,split.dec){
  
  n <- nrow(x)
  m <- ncol(x)
  
  d <- digits
  digits <- integer(m)
  digits[] <- d
  
  fo <- format
  format <- integer(m)
  format[] <- fo
  
  total.width <- max(ii)
  
  body <- array("",dim=c(nrow(x),total.width))
  
  for(i in seq(along=digits)){
    tmp <- formatC(x[,i],digits=digits[i],format=format[i])
    tmp <- gsub("-","&minus;",tmp,fixed=TRUE)
    if(!split.dec)
      body[,ii[i]] <- mk_td(tmp)
    else {
      tmp <- matrix(spltDec(tmp),ncol=3,byrow=TRUE)
      tmp <- mk_td_spltDec(tmp)
      body[,ii[i]] <- apply(tmp,1,paste0,collapse="")
    }
      
  }
  body
}

