df_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="0px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px"
)



format_html.data.frame <- function(x,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               row.names=TRUE,
                               digits=getOption("digits"),
                               format="f",
                               ...){

  
  style <- df_format_stdstyle
  firstcol <- c("padding-left"="0.3em")
  lastcol <- c("padding-right"="0.3em")
  toprule <- c("border-top"=paste0(midrule,"px solid"))
  bottomrule <- c("border-bottom"=paste0(midrule,"px solid"))
  midrule_above <- c("border-top"=paste0(midrule,"px solid"))
  midrule <- c("border-bottom"=paste0(midrule,"px solid"))
  align.right <- c("text-align"="right")  
  align.left <- c("text-align"="left")  
  align.center <- c("text-align"="center")
  
  colsep <- ""
  rowsep <- "\n"
  
  n <- nrow(x)
  m <- ncol(x)
  d <- digits
  is.int <- sapply(x,is.integer)
  is.num <- sapply(x,is.numeric) & !is.int
  m.num <- sum(is.num)
  digits <- integer(m.num)
  digits[] <- d
  fdigits <- integer(m)
  fdigits[is.num] <- digits
  fo <- format
  format <- character(m)
  format[is.num] <- fo  
  
  colspan <- integer(0)
  body <- matrix(nrow=nrow(x),ncol=0)
  for(i in 1:m) {
    cellstyle <- style
    if(i==1&&!row.names)
      cellstyle <- upd_vect(cellstyle,firstcol)
    if(i==m)
      cellstyle <- upd_vect(cellstyle,lastcol)
    if(is.int[i]){
      tmp <- formatC(x[,i],format="d")
      col <- c(mk_td(tmp[-n],style=proc_style(cellstyle)),
               mk_td(tmp[n],style=proc_style(upd_vect(cellstyle,bottomrule))))
      colspan <- c(colspan,1L)
      }
    else if(is.num[i]){
      tmp <- formatC(x[,i],digits=fdigits[i],format=format[i])
      if(split.dec){
        tmp <- t(matrix(spltDec(tmp),nrow=3))
        col <- rbind(mk_td_spltDec(tmp[-n,,drop=FALSE],style=proc_style(cellstyle)),
                     mk_td_spltDec(tmp[n,,drop=FALSE],style=proc_style(upd_vect(cellstyle,bottomrule))))
        colspan <- c(colspan,3L)
      }
      else{
        col <- c(mk_td(tmp[-n],style=proc_style(cellstyle)),
                 mk_td(tmp[n],style=proc_style(upd_vect(cellstyle,bottomrule))))
        colspan <- c(colspan,1L)
      }
    }
    else {
      tmp <- as.character(x[,i])
      col <- c(mk_td(tmp[-n],style=proc_style(cellstyle)),
               mk_td(tmp[n],style=proc_style(upd_vect(cellstyle,bottomrule))))
      colspan <- c(colspan,1L)
    }
    body <- cbind(body,col)
  }

  if(row.names){
    tmp <- rownames(x)
    rnstyle <- upd_vect(style,firstcol)
    ldr <- c(mk_td(tmp[-n],style=proc_style(rnstyle)),
             mk_td(tmp[n],style=proc_style(upd_vect(rnstyle,bottomrule))))
    body <- cbind(ldr,body)
  }
    
  body <- apply(body,1,paste0,collapse="")
  body <- mk_tr(body)
  
  hstyle <- upd_vect(style,align.center,toprule,midrule)
  hdr <- colnames(x)
  if(row.names) {
    hdr <- c("",hdr)
    colspan <- c(1L,colspan)
  }
  hdr <- mk_td(hdr,style=proc_style(hstyle),attribs=list(colspan=colspan))
  hdr <- mk_tr(paste0(hdr,collapse=""))
  ans <- c("<table class=\"mtable\" style=\"border-collapse: collapse;\">",
           hdr,
           body,
           "</table>")
  
  ans <- paste0(ans,collapse="\n")
  return(ans)
}


