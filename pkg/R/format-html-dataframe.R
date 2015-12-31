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
                               style=df_format_stdstyle,
                               margin="2ex auto",
                               ...){

  
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
    if(is.int[i]){
      tmp <- formatC(x[,i],format="d")
      col <- html_td(tmp,vectorize=TRUE,style=css(style))
      colspan <- c(colspan,1L)
      }
    else if(is.num[i]){
      tmp <- formatC(x[,i],digits=fdigits[i],format=format[i])
      if(split.dec){
        tmp <- spltDec(tmp)
        col <- html_td_spltDec(tmp,style=css(style))
        colspan <- c(colspan,3L)
      }
      else{
        col <- html_td(tmp,vectorize=TRUE,style=css(style))
        colspan <- c(colspan,1L)
      }
    }
    else {
      tmp <- as.character(x[,i])
      col <- html_td(tmp,vectorize=TRUE,style=css(style))
      col <- setStyle(col,align.left)
      colspan <- c(colspan,1L)
    }
    body <- cbind(body,col)
  }
  
  if(row.names){
    tmp <- rownames(x)
    ldr <- html_td(tmp,vectorize=TRUE,style=css(c(style,firstcol,align.right)))
    body <- cbind(ldr,body)
  }
   
  body[1,] <- lapply(body[1,],setStyle,toprule)
  body[n,] <- lapply(body[n,],setStyle,bottomrule)
  body <- apply(body,1,html_tr)
  
  hdr <- colnames(x)
  if(row.names) {
    hdr <- c("",hdr)
    colspan <- c(1L,colspan)
  }
  
  hdr <- html_td(hdr,vectorize=TRUE,style=css(style))
  hdr[] <- mapply(setAttribs,hdr,colspan=colspan,SIMPLIFY=FALSE)
  hdr <- lapply(hdr,setStyle,df_format_stdstyle)
  hdr <- lapply(hdr,setStyle,align.center)
  hdr <- lapply(hdr,setStyle,toprule)
  hdr[[1]] <- setStyle(hdr[[1]],lastcol)
  hdr[[length(hdr)]] <- setStyle(hdr[[length(hdr)]],lastcol)
  hdr <- html_tr(hdr)
  
  table_style <- c("border-collapse"="collapse")
  if(length(margin))
    table_style <- c(table_style,margin=margin)
  ans <- html_table(c(list(hdr),body),style=as.css(table_style))

  ans <- as.character(ans)
  return(ans)
}


