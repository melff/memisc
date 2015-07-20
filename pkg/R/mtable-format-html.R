mtable_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="0px",
  "margin-top"="0px",
  "margin-bottom"="0px"
)

mtable_format_html <- function(x,
                               interaction.sep = " &times; ",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               ...
){
  
  style <- mtable_format_stdstyle
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
  
  coef.dims <- lapply(coefs,dim)
  coef.ldim <- sapply(coef.dims,length)
  max.coef.ldim <- max(coef.ldim)
  
  coef.dims1 <- unique(sapply(coef.dims,"[[",1))
  stopifnot(length(coef.dims1)==1)

  coef.names <- dimnames(coefs[[1]])[[3]]  
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  mtab <- character()
  
  frmt1 <- function(name,coefs,summaries,is.last){
    
    coef.tab <- ftable(coefs,row.vars=c(3,1))
    coef.tab[] <- gsub("-","&minus;",coef.tab[],fixed=TRUE)
    #coef.tab[] <- gsub("([*]+)","<sup>\\1</sup>",coef.tab[]) # looks ugly ...
    if(split.dec){
      
      coef.tab <- t(apply(coef.tab,1,spltDec))
      colspan <- ncol(coef.tab)
      
      coef.tab[] <- mk_td_spltDec(coef.tab[], style=proc_style(style))
      neq <- ncol(coef.tab)/3
      dim(coef.tab) <- c(nrow(coef.tab),3,neq)
      coef.tab <- apply(coef.tab,c(1,3),paste0,collapse="")
      coef.tab[] <- gsub("([*]+)","<span class=\"signif.symbol\">\\1</span>",coef.tab[])
      
      
      if(max.coef.ldim>3){
        if(length(dim(coefs))>3 && dim(coefs)[4]>1)
          eq.names <- dimnames(coefs)[[4]]
        else
          eq.names <- ""
        attribs <- list(colspan=3*ncol(coef.tab)/length(eq.names))
        hstyle <- upd_vect(style,align.center,midrule)
        eq.names <- mk_td(eq.names,style=proc_style(hstyle),attribs=attribs)
        coef.tab <- rbind(eq.names,coef.tab)
      }
      
      tmp.smry <- summaries
      n <- length(tmp.smry)
      
      summaries <- matrix("",nrow=length(summaries),ncol=neq)
      summaries[,1] <- tmp.smry
      summaries <- t(apply(summaries,1,spltDec))
      
      summaries[1,] <- mk_td_spltDec(summaries[1,],
                                    style=proc_style(upd_vect(style,midrule_above)))
      summaries[-c(1,n),] <- mk_td_spltDec(summaries[-c(1,n),],
                                          style=proc_style(style))
      summaries[n,] <- mk_td_spltDec(summaries[n,],
                                    style=proc_style(upd_vect(style,bottomrule)))
      dim(summaries) <- c(nrow(summaries),3,neq) 
      summaries <- apply(summaries,c(1,3),paste0,collapse="")
    }
    else{
      
      colspan <- ncol(coef.tab)    
      coef.tab[] <- mk_td(coef.tab,style=proc_style(upd_vect(style,lrpad)))
      coef.tab[] <- gsub("([*]+?)","<span class=\"signif.symbol\">\\1</span>",coef.tab[])
      
      neq <- ncol(coef.tab)
      
      if(max.coef.ldim>3){
        if(length(dim(coefs))>3 && dim(coefs)[4]>1)
          eq.names <- dimnames(coefs)[[4]]
        else
          eq.names <- ""
        hstyle <- upd_vect(style,align.center,midrule)
        eq.names <- mk_td(eq.names,style=proc_style(hstyle))
        coef.tab <- rbind(eq.names,coef.tab)
      }
      
      tmp.smry <- summaries
      summaries <- matrix("",nrow=length(summaries),ncol=neq)
      summaries[,1] <- tmp.smry
      n <- length(tmp.smry)
      
      summaries[1,] <- mk_td(summaries[1,],
                            style=proc_style(upd_vect(style,midrule_above)))
      summaries[-c(1,n),] <- mk_td(summaries[-c(1,n),],
                                  style=proc_style(style))
      summaries[n,] <- mk_td(summaries[n,],
                            style=proc_style(upd_vect(style,bottomrule)))
      
    }
    coef.tab <- apply(coef.tab,1,paste0,collapse="")
    summaries <- apply(summaries,1,paste0,collapse="")
    hstyle <- upd_vect(style,align.center,toprule)
    if(length(dim(coefs))>3 && dim(coefs)[4]>1 || max.coef.ldim==3){
      hstyle <- upd_vect(hstyle,midrule)
    }
    header <- mk_td(name,style=proc_style(hstyle),attribs=list(colspan=colspan))
    c(header,coef.tab,summaries)
  }
  for(n in names(coefs)){
    mtab <- cbind(mtab,frmt1(n,coefs[[n]],summaries[,n]))
  }
  
  
  ldr <- character(nrow(mtab))
  
  ii.coef <- seq(from=1,by=coef.dims1,length=length(coef.names))
  ii.smry <- seq(from=1+length(coef.names)*coef.dims1,length=nrow(summaries))
  
  if(max.coef.ldim==4) {
    ii.coef <- ii.coef + 2 
    ii.smry <- ii.smry + 2 
  }
  else {
    ii.coef <- ii.coef +1
    ii.smry <- ii.smry +1
  }

  ldr[ii.coef] <- coef.names
  ldr[ii.smry] <- rownames(summaries)
  lstyle <- upd_vect(style,align.left,firstcol)
  if(ii.coef[1]-1==1){
    lstyle.tmp <- upd_vect(lstyle,toprule,midrule)
    ldr[1] <- mk_td(ldr[1],style=proc_style(lstyle.tmp))
  }
  else {
    lstyle.tmp <- upd_vect(lstyle,toprule)
    ldr[1] <- mk_td(ldr[1],style=proc_style(lstyle.tmp))
    lstyle.tmp <- upd_vect(lstyle,midrule)
    ldr[ii.coef[1]-1] <- mk_td(ldr[ii.coef[1]-1],style=proc_style(lstyle.tmp))
  }
  lstyle.tmp <- upd_vect(lstyle,midrule)
  ldr[ii.smry[1]-1] <- mk_td(ldr[ii.smry[1]-1],style=proc_style(lstyle.tmp))
  lstyle.tmp <- upd_vect(lstyle,bottomrule)
  ldr[length(ldr)] <- mk_td(ldr[length(ldr)],style=proc_style(lstyle.tmp))
  if(ii.coef[1]-1==1)
    ii <- c(1,ii.smry[1]-1,length(ldr))
  else
    ii <- c(1,ii.coef[1]-1,ii.smry[1]-1,length(ldr))
  ldr[-ii] <- mk_td(ldr[-ii],style=proc_style(lstyle))
  
  body <- cbind(ldr,mtab)
  body <- apply(body,1,paste0,collapse="")
  body <- mk_tr(body)
  
  ans <- c("<table class=\"mtable\" style=\"border-collapse: collapse;\">",
           body,
           "</table>")
  
  ans <- paste0(ans,collapse="\n")
  return(ans)
}



format_html.mtable <- function(x,
                               interaction.sep = " &times; ",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,...)
  mtable_format_html(x,interaction.sep=interaction.sep,
                     toprule=toprule,midrule=midrule,bottomrule=bottomrule,
                     split.dec=split.dec,...)
