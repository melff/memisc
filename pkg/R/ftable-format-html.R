ftable_format_stdstyle <- c(
  "padding-top"="0px",
  "padding-bottom"="0px",
  "margin-top"="0px",
  "margin-bottom"="0px"
)


format_html.ftable <- function(x,
                               show.titles=TRUE,
                               digits=0,
                               format="f",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,...){
  
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
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n <- nrow(x)
  m <- ncol(x)
  d <- digits
  digits <- integer(m)
  digits[] <- d
  
  fo <- format
  format <- integer(m)
  format[] <- fo
  
  body <- array("",dim=dim(x))
  for(i in seq(along=digits)) {
    #print(digits[i])
    body[,i] <- formatC(x[,i],digits=digits[i],format=format[i])
  }
  
  body <- array(trimws(body),dim=dim(x))
  body[] <- gsub("-","&minus;",body[],fixed=TRUE)
  if(split.dec){
    body <- t(apply(body,1,spltDec))
  }
  
  if(split.dec){
    body[-n,] <- mk_td_spltDec(body[-n,], style=proc_style(style))
    body[n,] <- mk_td_spltDec(body[n,], style=proc_style(upd_vect(style,bottomrule)))
  }
  else {
    bstyle <- upd_vect(style,lrpad)
    body[-n,] <- mk_td(body[-n,], style=proc_style(upd_vect(bstyle,align.right)))
    body[n,] <- mk_td(body[n,], style=proc_style(upd_vect(bstyle,align.right,bottomrule)))
  }
  
  leaders <- array("",dim=c(n,n.row.vars))
  if(show.titles)
    leaders <- cbind(leaders,"")

  mm <- 1
  for(j in rev(1:n.row.vars)){
    rv <-row.vars[[j]]
    nrv <- length(rv)
    nn <- n/mm
    i <- (1:nn)*mm - mm + 1
    leaders[i,j] <- rv
    mm <- mm*nrv
  }
  for(i in 1:n){
    lstyle <- style
    if(i == 1) lstyle <- upd_vect(lstyle,toprule)
    if(i == nrow(leaders)) lstyle <- upd_vect(lstyle,bottomrule)
    lstyle1 <- upd_vect(lstyle,first.col)
    lstyle2 <- lstyle
    
    if(ncol(leaders)>1)
      lstyle <- c(lstyle1,rep(lstyle2,ncol(leaders)-1))
    else
      lstyle <- lstyle1
    leaders[i,] <- mk_td(leaders[i,],style=proc_style(lstyle)) 
  }
  
  body <- cbind(leaders,body)
  body <- apply(body,1,paste0,collapse="")
  body <- mk_tr(body)
  
  header <- list()
  mm <- 1
  for(i in rev(1:n.col.vars)){
    cv <- col.vars[[i]]
    ncv <- length(cv)
    if(split.dec)
      attribs <- list(colspan=mm*3)
    else
      attribs <- list(colspan=mm)
    mm <- mm*ncv
    cv <- rep(cv,m%/%mm)
    
    hstyle <- upd_vect(style,align.center,lrpad)
    if(i == 1 && !(show.titles && n.col.vars == 1))
      hstyle <- upd_vect(hstyle,toprule)
    if(i == n.col.vars)
      hstyle <- upd_vect(hstyle,midrule)
    if(any(nzchar(hstyle)))
      
    
    if(show.titles){
      if(n.col.vars == 1){
        hstyle <- upd_vect(hstyle,bottomrule)
        
        if(!nzchar(names(col.vars)))
          hstyle <- upd_vect(hstyle,toprule)
        
        htmp1 <- mk_td(c(names(row.vars),""),
                       style=proc_style(upd_vect(hstyle,align.left)))
      }
      else {
        if(i == n.col.vars){
          htmp1 <- mk_td(c(names(row.vars),names(col.vars)[i]),
                         style=proc_style(upd_vect(hstyle,align.left)))
        }
        else
          htmp1 <- mk_td(c(rep("",n.row.vars),names(col.vars)[i]),
                         style=proc_style(upd_vect(hstyle,align.left)))
      }      
    }
    else 
      htmp1 <- mk_td(rep("",ncol(leaders)),style=proc_style(hstyle))
    
    attribs$style <- proc_style(hstyle)
    htmp2 <- mk_td(cv,attribs=attribs)
    header <- c(list(c(htmp1,htmp2)),header)
  }
  if(show.titles && n.col.vars == 1){
    if(nzchar(names(col.vars))){
      hstyle <- upd_vect(style,toprule,lrpad)
      htmp1 <- mk_td(rep("",ncol(leaders)),
                     style=proc_style(hstyle))
      colspan <- ncol(x)
      if(split.dec) 
        colspan <- colspan*3
      attribs <- list(colspan=colspan,
                      style=proc_style(upd_vect(hstyle,align.center,midrule)))
      htmp2 <- mk_td(names(col.vars),attribs=attribs)
      header <- c(list(c(htmp1,htmp2)),header)
    }
  }
  header <- sapply(header,paste0,collapse="")
  header <- mk_tr(header)

  ans <- c("<table class=\"mtable\" style=\"border-collapse: collapse;\">",
           header,
           body,
           "</table>")
  
  ans <- paste0(ans,collapse="\n")
  return(ans)
}

