mtable_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="0px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px"
)

mtable_format_html <- function(x,
                               interaction.sep = NULL,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               margin="2ex auto",
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
  
  if(!length(interaction.sep)){
    if(getOption("html.use.ampersand",FALSE))
      interaction.sep <- " &times; "
    else 
      interaction.sep <- " \u00d7 "
  }
  
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
  
  grp.coefs <- max.coef.ldim > 3 
  if(grp.coefs){
    coef.dims4 <- sapply(coef.dims[coef.ldim>3],"[",4)
    grp.coefs <- grp.coefs && any(coef.dims4>1)
  }

  coef.names <- dimnames(coefs[[1]])[[3]]  
  if(interaction.sep !=" x ")
    coef.names <- gsub(" x ",interaction.sep,coef.names,fixed=TRUE)
  
  mtab <- list()
  
  frmt1 <- function(name,coefs,summaries,is.last){
    
    coef.tab <- coefs
    if(getOption("html.use.ampersand",FALSE))
      coef.tab[] <- gsub("-","&minus;",coef.tab[],fixed=TRUE)
    else
      coef.tab[] <- gsub("-","\u2212",coef.tab[],fixed=TRUE)
    #coef.tab[] <- gsub("([*]+)","<sup>\\1</sup>",coef.tab[]) # looks ugly ...
    
    dm <- dim(coefs)
    if(length(dm)==3) dm <- c(dm,1)
    neq <-  dm[4]
    
    if(split.dec){
      coef.tab <- spltDec(coef.tab)
      coef.tab <- gsub("([*]+)","<span class=\"signif.symbol\">\\1</span>",coef.tab)
      coef.tab <- html_td_spltDec(coef.tab, style=css(style))
    }
    else
      coef.tab <- html_td(coef.tab, style=css(style),vectorize=TRUE)
    
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
    if(grp.coefs){
      if(dm[4]>1)
        eq.names <- dimnames(coefs)[[4]]
      else
        eq.names <- ""
      
      hstyle <- upd_vect(style,align.center,midrule)
      eq.names <- html_td(eq.names,colspan=colspan,style=css(hstyle),vectorize=TRUE)
      coef.tab <- rbind(eq.names,coef.tab)
      colspan <- colspan*neq
    }
    dms <- c(1,dm[2],length(summaries),dm[4])
    sum.tab <- array("",dim=dms)
    sum.tab[1,1,,1] <- summaries
    sum.tab[] <- gsub("-","&minus;",sum.tab[],fixed=TRUE)
    
    if(split.dec){
      sum.tab <- spltDec(sum.tab)
      sum.tab <- html_td_spltDec(sum.tab, style=css(style))
    }
    else
      sum.tab <- html_td(sum.tab, style=css(style),vectorize=TRUE)
    
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
      
      hstyle <- upd_vect(style,align.center,midrule)
      header <- html_td(name,colspan=colspan,style=css(hstyle))
      mtab <- c(list(header),mtab)
    }
    
    mtab[1] <- lapply(mtab[1],setStyle,toprule)
    mtab
  }
  
  if(length(x$model.groups)){
    for(i in seq_along(x$model.groups)){
      
      mg <- x$model.groups[[i]]
      mtab.m <- character()
      colspan <- 0
      
      for(j in mg){
        dm <- dim(coefs)[[j]]
        colspan.j <- prod(dm[c(2,4)])
        if(split.dec)
          colspan.j <- 3*colspan.j
        colspan <- colspan + colspan.j
        mtab.m <- cbind(mtab.m,frmt1(names(coefs)[j],coefs[[j]],summaries[,j]))
      }
      
      mtab.m <- apply(mtab.m,1,as.html_group)
      model.name <- names(x$model.groups)[i]
      hstyle <- upd_vect(style,align.center,midrule,toprule)
      model.name <- html_td(model.name,colspan=colspan,style=css(hstyle))
      mtab.m <- c(list(model.name),mtab.m)
      mtab <- cbind(mtab,mtab.m)
    }
  }
  else {
    for(i in 1:length(coefs)){
      mtab <- cbind(mtab,frmt1(names(coefs)[i],coefs[[i]],summaries[,i]))
    }
  }
  
  dm <- coef.dims[[1]]
  ldr <- character(length=dm[1]*dm[3])
  ii.coef <- seq(from=1,by=dm[1],length=dm[3])
  ldr[ii.coef] <- coef.names
  lstyle <- upd_vect(style,align.left,firstcol)
  ldr <- html_td(ldr,vectorize=TRUE,style=css(lstyle))
  ldr[length(ldr)] <- setStyle(ldr[length(ldr)],midrule)

  sldr <- html_td(rownames(summaries),vectorize=TRUE,style=css(lstyle))
  sldr[length(sldr)] <- setStyle(sldr[length(sldr)],bottomrule)
  ldr <- c(ldr,sldr)
  
  hldr <- NULL
  if(num.models > 1 || force.names || max.coef.ldim > 3){
    
    hldr <- rep("",(num.models > 1 || force.names)+as.integer(grp.coefs)
                    + as.integer(length(x$model.groups)>0))
    hldr <- html_td(hldr,vectorize=TRUE,style=css(lstyle))
    hldr[length(hldr)] <- setStyle(hldr[length(hldr)],midrule)
    hldr[1] <- setStyle(hldr[1],toprule)
    ldr <- c(hldr,ldr)
  } 
  body <- cbind(ldr,mtab)
  body <- apply(body,1,as.html_group)
  body <- html_tr(body,vectorize=TRUE)
  
  table_style <- c("border-collapse"="collapse")
  if(length(margin))
    table_style <- c(table_style,margin=margin)
  ans <- html_table(body,class="mtable",style=as.css(table_style))
  
  ans <- as.character(ans)
  return(ans)
}



format_html.mtable <- function(x,
                               interaction.sep = NULL,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               margin="2ex auto",
                               force.names=FALSE,...)
  mtable_format_html(x,interaction.sep=interaction.sep,
                     toprule=toprule,midrule=midrule,bottomrule=bottomrule,
                     split.dec=split.dec,style=style,margin=margin,
                     force.names=force.names,...)
