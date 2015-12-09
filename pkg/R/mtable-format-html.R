mtable_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="0px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px"
)

mtable_format_html <- function(x,
                               interaction.sep = " &times; ",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               force.names = FALSE,
                               ...
){
  
  firstcol <- c("padding-left"="0.3em")
  toprule <- c("border-top"=paste0(midrule,"px solid"))
  bottomrule <- c("border-bottom"=paste0(midrule,"px solid"))
  midrule_above <- c("border-top"=paste0(midrule,"px solid"))
  midrule <- c("border-bottom"=paste0(midrule,"px solid"))
  align.right <- c("text-align"="right")  
  align.left <- c("text-align"="left")  
  align.center <- c("text-align"="center")
  lrpad <- c("padding-left"="0.3em","padding-right"="0.3em")
  
  colsep <- ""
  rowsep <- "\n"

  coefs <- x$coefficients
  summaries <- x$summaries
  
  num.models <- length(coefs)
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  max.coef.ldim <- max(coef.ldim)
  
  coef.dims1 <- unique(sapply(coef.dims,"[[",1))
  stopifnot(length(coef.dims1)==1)

  coef.names <- dimnames(coefs[[1]])[[3]]  
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  mtab <- list()
  
  frmt1 <- function(name,coefs,summaries,is.last){
    
    coef.tab <- coefs
    coef.tab[] <- gsub("-","&minus;",coef.tab[],fixed=TRUE)
    #coef.tab[] <- gsub("([*]+)","<sup>\\1</sup>",coef.tab[]) # looks ugly ...
    
    dm <- dim(coefs)
    if(length(dm)==3) dm <- c(dm,1)
    neq <-  dm[4]
    
    if(split.dec){
      coef.tab <- spltDec(coef.tab)
      coef.tab <- gsub("([*]+)","<span class=\"signif.symbol\">\\1</span>",coef.tab)
      coef.tab <- html_td_spltDec(coef.tab, style=html_style(style))
    }
    else
      coef.tab <- html_td(coef.tab, style=html_style(style),vectorize=TRUE)
    
    dim(coef.tab) <- dm
    if(dm[2]==1)
      dim(coef.tab) <- c(prod(dm[c(1,3)]),dm[4])
    else{
      coef.tab <- apply(coef.tab,c(1,3,4),as.html_group)
      dim(coef.tab) <- c(prod(dm[c(1,3)]),dm[4])
    }
      
    colspan <- dm[2]
    if(split.dec)
      colspan <- 3*colspan
    if(max.coef.ldim>3){
      if(dm[4]>1)
        eq.names <- dimnames(coefs)[[4]]
      else
        eq.names <- ""
      
      hstyle <- upd_vect(style,align.center,midrule)
      eq.names <- html_td(eq.names,colspan=colspan,style=html_style(hstyle),vectorize=TRUE)
      coef.tab <- rbind(eq.names,coef.tab)
      colspan <- colspan*neq
    }
    dms <- c(1,dm[2],length(summaries),dm[4])
    sum.tab <- array("",dim=dms)
    sum.tab[1,1,,1] <- summaries
    sum.tab[] <- gsub("-","&minus;",sum.tab[],fixed=TRUE)
    
    if(split.dec){
      sum.tab <- spltDec(sum.tab)
      sum.tab <- html_td_spltDec(sum.tab, style=html_style(style))
    }
    else
      sum.tab <- html_td(sum.tab, style=html_style(style),vectorize=TRUE)
    
    dim(sum.tab) <- dms
    if(dms[2]==1)
      dim(sum.tab) <- c(prod(dms[c(1,3)]),dms[4])
    else{
      sum.tab <- apply(sum.tab,c(1,3,4),as.html_group)
      dim(sum.tab) <- c(prod(dms[c(1,3)]),dms[4])
    }
    
    nn <- nrow(coef.tab)
    coef.tab[nn,] <- lapply(coef.tab[nn,],setStyle,midrule)
    nn <- nrow(sum.tab)
    sum.tab[nn,] <- lapply(sum.tab[nn,],setStyle,midrule)
    
    coef.tab <- apply(coef.tab,1,as.html_group)
    sum.tab <- apply(sum.tab,1,as.html_group)
    
    mtab <- c(coef.tab,sum.tab)
    
    if(num.models>1 || force.names){
      
      hstyle <- upd_vect(style,align.center)
      if(length(dim(coefs))>3 && dim(coefs)[4]>1 || max.coef.ldim==3){
        hstyle <- upd_vect(hstyle,midrule)
      }
      header <- html_td(name,colspan=colspan,style=html_style(hstyle))
      mtab <- c(html_group(header),mtab)
    }
    
    mtab[1] <- lapply(mtab[1],setStyle,toprule)
    mtab
  }
  
  
  for(n in names(coefs)){
    mtab <- cbind(mtab,frmt1(n,coefs[[n]],summaries[,n]))
  }
  
  dm <- coef.dims[[1]]
  ldr <- character(length=dm[1]*dm[3])
  ii.coef <- seq(from=1,by=dm[1],length=dm[3])
  ldr[ii.coef] <- coef.names
  lstyle <- upd_vect(style,align.left,firstcol)
  ldr <- html_td(ldr,vectorize=TRUE,style=html_style(lstyle))
  ldr[length(ldr)] <- setStyle(ldr[length(ldr)],midrule)

  sldr <- html_td(rownames(summaries),vectorize=TRUE,style=html_style(lstyle))
  sldr[length(sldr)] <- setStyle(sldr[length(sldr)],bottomrule)
  ldr <- c(ldr,sldr)
  
  hldr <- NULL
  if(num.models > 1 || force.names || max.coef.ldim > 3){
    
    hldr <- rep("",(num.models > 1 || force.names)+(max.coef.ldim > 3))
    hldr <- html_td(hldr,vectorize=TRUE,style=html_style(lstyle))
    hldr[length(hldr)] <- setStyle(hldr[length(hldr)],midrule)
    hldr[1] <- setStyle(hldr[1],toprule)
    ldr <- c(hldr,ldr)
  } 
  body <- cbind(ldr,mtab)
  body <- apply(body,1,as.html_group)
  body <- html_tr(body,vectorize=TRUE)
  
  ans <- html_table(body,class="mtable",style=html_style("border-collapse"="collapse"))
  
  ans <- as.character(ans)
  return(ans)
}



format_html.mtable <- function(x,
                               interaction.sep = " &times; ",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,...)
  mtable_format_html(x,interaction.sep=interaction.sep,
                     toprule=toprule,midrule=midrule,bottomrule=bottomrule,
                     split.dec=split.dec,...)
