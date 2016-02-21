format_html.ftable_matrix <- function(x,
                               show.titles=TRUE,
                               digits=0,
                               format="f",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=ftable_format_stdstyle,
                               margin="2ex auto",
                               varontop,varinfront,
                               ...){
  
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
  
  l.cv <- sapply(col.vars,length)
  max.l.cv <- max(l.cv)
  
  l.rv <- sapply(row.vars,length)
  max.l.rv <- max(l.rv)
  
  if(missing(varontop)) varontop <- (max.l.cv <= 1)
  if(missing(varinfront)) varinfront <- (max.l.rv <= 1)
  if(varontop) max.l.cv <- max.l.cv + 1
  
  d <- digits
  digits <- integer(M)
  digits[] <- d
  
  fo <- format
  format <- integer(M)
  format[] <- fo
  
  
  headers <- mapply(htfm_mkHeader,col.vars,m,
                    MoreArgs=list(max.l.cv,varontop,split.dec,style,midrule),SIMPLIFY=FALSE)
  leaders <- mapply(htfm_mkLeader,row.vars,n,
                    MoreArgs=list(max.l.rv,varinfront,style),SIMPLIFY=FALSE)

  body <- array(list(),dim=dim(x))
  
  mm <- length(headers[[1]])
  nn <- ncol(leaders[[1]])
  
  lheaders <- matrix("",nrow=mm,ncol=nn)
  lstyle <- upd_vect(style,align.left)
  
  for(i in 1:N){
    rv.i <- row.vars[[i]]

    if(l.rv[i] > 1 || (!varinfront && nzchar(names(rv.i)))){
      
      n.rv.i <- names(rv.i)
      if(l.rv[i] > 1 && varinfront)
        n.rv.i[1] <- ""

      if(i == 1)
        lheaders[mm,] <- n.rv.i
      else {
        tmp.ldr <- leaders[[i]]
        tmp.ldr <- rbind(html_td(n.rv.i,vectorize=TRUE),tmp.ldr)
        leaders[[i]] <- tmp.ldr
      }
    }
    if(i < N) {
      tmp.ldr <- leaders[[i]]
      nnn <- nrow(tmp.ldr)
      tmp.ldr[nnn,] <- lapply(tmp.ldr[nnn,],setStyle,midrule)
      leaders[[i]] <- tmp.ldr
    }
    
    for(j in 1:M){
      cv.j <- col.vars[[j]]
      tmp.bdy <- htfm_mkBody(x[[i,j]],format[[j]],digits[[j]],split.dec,style)
      if(i > 1 && (l.rv[i] > 1 || (!varinfront && nzchar(names(rv.i))))){

        if(split.dec){
          tmp. <- spltDec(rep("",ncol(tmp.bdy)))
          tmp.bdy <- rbind(html_td_spltDec(tmp.),tmp.bdy)
        }        
        else
          tmp.bdy <- rbind(html_td(rep("",ncol(tmp.bdy)),vectorize=TRUE),tmp.bdy)
      }
      if(l.cv[j] > 1 || (!varontop && nzchar(names(cv.j)))){
        
        tmp.bdy <- cbind(html_td(rep("",nrow(tmp.bdy)),vectorize=TRUE),tmp.bdy)
      }
      if(i < N){
        nnn <- nrow(tmp.bdy)
        tmp.bdy[nnn,] <- lapply(tmp.bdy[nnn,],setStyle,midrule)
      }
      body[[i,j]] <- tmp.bdy
    }
  }

  lheaders <- array(html_td(lheaders,vectorize=TRUE,style=css(lstyle)),
          dim=dim(lheaders))

  leaders <- rbind(lheaders,do.call(rbind,leaders))
    
  ans <- list()
  for(j in 1:M){
    ans.j <- body[,j]
    ans.j <- do.call(rbind,ans.j)
    ans.j <- apply(ans.j,1,as.html_group)
    ans[[j]] <- as.html_group(c(headers[[j]],ans.j))
  }
  ans <- do.call(cbind,ans)
  ans <- cbind(leaders,ans)
  mmm <- nrow(ans)
  ans[1,] <- lapply(ans[1,],setStyle,toprule)
  ans[mm,] <- lapply(ans[mm,],setStyle,midrule)
  ans[mmm,] <- lapply(ans[mmm,],setStyle,bottomrule)
  ans <- apply(ans,1,as.html_group)
  ans <- html_tr(ans,vectorize=TRUE)
  
  table_style <- c("border-collapse"="collapse")
  if(length(margin))
    table_style <- c(table_style,margin=margin)
  ans <- html_table(ans,class="ftable",style=as.css(table_style))

  ans <- as.character(ans)
  return(ans)
  
}


htfm_mkHeader <- function(col.vars,m,max.l.cv,varontop,split.dec,style,midrule){
 
  align.right <- c("text-align"="right")  
  align.left <- c("text-align"="left")  
  align.center <- c("text-align"="center")
 
  nms.cv <- names(col.vars)
  n.col.vars <- length(col.vars)
  
  hstyle <- upd_vect(style,align.center)
  header <- vector("list",length=n.col.vars)
  mm <- 1
  for(i in rev(1:n.col.vars)){
    
    cv <- col.vars[[i]]
    ncv <- length(cv)
    if(split.dec)
      colspan <- mm*3
    else
      colspan <- mm
    mm <- mm*ncv
    cv <- rep(cv,m%/%mm)
    
    header.i <- html_td(cv,vectorize=TRUE,style=css(hstyle),
                        colspan=colspan) 
    header.i <- setStyle(header.i,midrule)
    
    if(n.col.vars > 1 || !varontop){
      
      if(i == 1 && varontop || !length(nms.cv)){
        htmp <- html_td("",
                        style=css(upd_vect(hstyle,align.left)))
      }
      else {
        if(!nzchar(nms.cv[i])){
          htmp <- NULL
        }
        else
          htmp <- html_td(paste0(nms.cv[i],":"),
                          style=css(upd_vect(hstyle,align.left)))
      }    
      if(length(htmp))
        header.i <- c(htmp,header.i)
    }
    header[[i]] <- header.i
  }
  
  if(varontop){

    if(split.dec)
      colspan <- mm*3
    else
      colspan <- mm
    
    if(n.col.vars > 1){
      htmp1 <- html_td("",
                       style=css(upd_vect(hstyle)))
      htmp2 <- html_td(nms.cv[1],
                      style=css(upd_vect(hstyle,align.center,midrule)),
                      colspan=colspan)
      htmp <- c(htmp1,htmp2)
    }
    else if(nzchar(nms.cv[1])){
      htmp <- html_td(names(col.vars)[1],
                      style=css(upd_vect(hstyle,align.center,midrule)),
                      colspan=colspan)
    }
    else {
      htmp <- html_td("",
                      style=css(upd_vect(hstyle,align.center)),
                      colspan=colspan)
    }
    header <- c(list(htmp),header)
  }
  
  tmp.header <- header

  if(split.dec)
    colspan <- m*3
  else
    colspan <- m
  if(n.col.vars > 1 || !varontop)
    colspan <- colspan + 1
  
  header <- rep(list(html_td("",colspan=colspan)),max.l.cv)
  
  i <- seq(to=max.l.cv,length=length(tmp.header))
  header[i] <- tmp.header
  header
}

htfm_mkLeader <- function(row.vars,n,max.l.rv,varinfront,style) {
  
  align.left <- c("text-align"="left")  
  n.row.vars <- length(row.vars)
  nms.row.vars <- names(row.vars)
  lstyle <- upd_vect(style,align.left)
  leader <- matrix("",nrow=n,ncol=n.row.vars)
  
  for(ii in 1:n.row.vars){
    
    rv <- row.vars[[ii]]
    rep.rv <- if(ii==1)1 else nn.rv
    rv <- rep(rv,rep.rv)
    nn.rv <- length(rv)
    rv.n <- n/nn.rv
    pos <- (1:nn.rv)*rv.n
    pos <- pos - rv.n + 1
    leader[pos,ii] <- rv
  }
  if(varinfront){
    lleader <- character(n)
    if(nzchar(nms.row.vars[1]))
      lleader[1] <- nms.row.vars[1]
    else if(n==1){
      lleader[1] <- leader[1,1]
      leader[1,1] <- ""
    }
      
    leader <- cbind(lleader,leader)
    if(n.row.vars > 1){
      hleader <- character(n.row.vars+1)
      ii <- 2:n.row.vars
      hleader[ii] <- nms.row.vars[ii]
      leader <- rbind(hleader,leader)
    }
  }
  
  tmp.leader <- leader
  leader <- matrix("",nrow=nrow(leader),ncol=max.l.rv+as.integer(varinfront))
  ii <- seq(to=ncol(leader),length=ncol(tmp.leader))
  leader[,ii] <- tmp.leader
  
  leader <- array(html_td(leader,vectorize=TRUE,style=css(lstyle)),
                  dim=dim(leader))
  leader
}

htfm_mkBody <- function(x,format,digits,split.dec,style){
  
  n <- nrow(x)
  m <- ncol(x)
  
  d <- digits
  digits <- integer(m)
  digits[] <- d
  
  fo <- format
  format <- integer(m)
  format[] <- fo
  
  body <- array(list(),dim=c(n,m))
  
  for(i in 1:m){
    tmp <- formatC(x[,i],digits=digits[i],format=format[i])
    if(getOption("html.use.ampersand",FALSE))
      tmp <- gsub("-","&minus;",tmp,fixed=TRUE)
    else
      tmp <- gsub("-","\u2212",tmp,fixed=TRUE)
    if(!split.dec)
      body[,i] <- html_td(tmp,vectorize=TRUE,style=css(style))
    else {
      tmp <- spltDec(tmp)
      tmp <- html_td_spltDec(tmp,style=css(style))
      body[,i] <- tmp
    }
  }
  body
}

