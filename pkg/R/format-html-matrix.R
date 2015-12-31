mat_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="0px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px"
)



format_html.matrix <- function(x,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               digits=getOption("digits"),
                               format="f",
                               style=mat_format_stdstyle,
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

  colspan <- integer(m)
  body <- matrix(nrow=nrow(x),ncol=ncol(x))
  if(is.integer(x)){
    tmp <- formatC(x,format="d")
    body <- html_td(tmp,vectorize=TRUE)
    colspan <- 1L
    }
  else if(is.numeric(x)){
    tmp <- formatC(x,digits=digits,format=format)
    if(split.dec){
      tmp <- spltDec(tmp)
      body <- html_td_spltDec(tmp,style=css(style))
      dim(body) <- dim(x)
      colspan <- 3L
    }
    else{
      body <- html_td(tmp,vectorize=TRUE,style=css(style))
      dim(body) <- dim(x)
      colspan <- 1L
    }
  }
  else {
    tmp <- as.character(x)
    body <- html_td(tmp,vectorize=TRUE,style=css(style))
    colspan <- 1L
  }

  if(length(rownames(x))){
    tmp <- rownames(x)
    ldr <- html_td(tmp,vectorize=TRUE,style=css(c(style,firstcol,align.right)))
    body <- cbind(ldr,body)
  }
   
  body[1,] <- lapply(body[1,],setStyle,toprule)
  body[n,] <- lapply(body[n,],setStyle,bottomrule)
  
  body <- apply(body,1,html_tr)
  
  if(length(colnames(x))){
    
    hdr <- colnames(x)
    if(length(rownames(x))){
      hdr <- c("",hdr)
      colspan <- c(1L,rep(colspan,m))
    }
    else
      colspan <- rep(colspan,m)
    hdr <- html_td(hdr,vectorize=TRUE,style=css(style))
    hdr[] <- mapply(setAttribs,hdr,colspan=colspan,SIMPLIFY=FALSE)
    hdr <- lapply(hdr,setStyle,align.center)
    hdr <- lapply(hdr,setStyle,toprule)
    hdr[[length(hdr)]] <- setStyle(hdr[[length(hdr)]],lastcol)
    hdr <- html_tr(hdr)
    
    ans <- html_table(c(list(hdr),body))
  }
  else
    ans <- html_table(body)
  
  table_style <- c("border-collapse"="collapse")
  if(length(margin))
    table_style <- c(table_style,margin=margin)
  style(ans) <- as.css(table_style)

  ans <- as.character(ans)
  return(ans)
}


