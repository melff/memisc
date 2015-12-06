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
    tmp <- spltDec(body)
    body <- html_td_spltDec(tmp)
    dim(body) <- c(3,n,m)
    body <- aperm(body,c(2,1,3))
    dim(body) <- c(n,3*m)
    colspan <- 3L
  }
  else {
    body <- html_td(body,vectorize=TRUE)
    dim(body) <- dim(x)
    colspan <- 1L
  }
  
  leaders <- array(list(),dim=c(n,n.row.vars))
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
    lstyle1 <- upd_vect(lstyle,first.col)
    lstyle2 <- lstyle
    
    if(ncol(leaders)>1)
      lstyle <- c(lstyle1,rep(lstyle2,ncol(leaders)-1))
    else
      lstyle <- lstyle1
    
    leaders[i,] <- html_td(leaders[i,],style=html_style(lstyle),
                           vectorize=TRUE) 
  }
  
  body <- cbind(leaders,body)
  nn <- nrow(body)
  body[nn,] <- lapply(body[nn,],setStyle,bottomrule)
  body <- apply(body,1,html_tr)
  
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

    if(show.titles){
      if(n.col.vars == 1){
        htmp1 <- html_td(c(names(row.vars),""),
                         style=html_style(upd_vect(hstyle,align.left)),
                         vectorize=TRUE)
      }
      else {
        if(i == n.col.vars){
          htmp1 <- html_td(c(names(row.vars),names(col.vars)[i]),
                         style=html_style(upd_vect(hstyle,align.left)),
                         vectorize=TRUE)
        }
        else
          htmp1 <- html_td(c(rep("",n.row.vars),names(col.vars)[i]),
                         style=html_style(upd_vect(hstyle,align.left)),
                         vectorize=TRUE)
      }      
    }
    else 
      htmp1 <- html_td(rep("",ncol(leaders)),style=html_style(hstyle),
                       vectorize=TRUE)
    
    if(i==n.col.vars)
      attribs$style <- html_style(hstyle)
    else
      attribs$style <- html_style(upd_vect(hstyle,midrule))
      
    htmp2 <- lapply(html_td(cv,vectorize=TRUE),setAttribs,attribs)
    header <- c(list(c(htmp1,htmp2)),header)
  }
  if(show.titles && n.col.vars == 1){
    if(nzchar(names(col.vars))){
      hstyle <- upd_vect(style,lrpad)
      htmp1 <- html_td(rep("",ncol(leaders)),
                       style=html_style(hstyle),
                       vectorize=TRUE)
      colspan <- ncol(x)
      if(split.dec) 
        colspan <- colspan*3
      attribs <- list(colspan=colspan,
                      style=html_style(upd_vect(hstyle,align.center,midrule)))
      htmp2 <- lapply(html_td(names(col.vars),vectorize=TRUE),setAttribs,attribs)
      header <- c(list(c(htmp1,htmp2)),header)
    }
  }
  header[[1]] <- lapply(header[[1]],setStyle,toprule)
  lh <- length(header)
  header[[lh]] <- lapply(header[[lh]],setStyle,midrule)
  
  header <- html_tr(header,vectorize=TRUE)

  ans <- html_table(c(header,body),class="ftable",style=html_style("border-collapse"="collapse"))
  
  ans <- as.character(ans)
  return(ans)
}

