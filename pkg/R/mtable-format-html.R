mtable_format_stdstyle <- c(
  "padding-top"="0px",
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
  
  coldims <- dim(x$coefficients)[x$as.col]
  nhrows <- length(coldims)
  
  coefnames <- dimnames(x$coefficients)[[x$coef.dim]]
  if(interaction.sep !=" x ")
    coefnames <- gsub(" x ",interaction.sep,coefnames,fixed=TRUE)
  dimnames(x$coefficients)[[x$coef.dim]] <- coefnames
  coefs <- ftable(as.table(x$coefficients),row.vars=rev(x$as.row),
                  col.vars=rev(x$as.col)
  )
  infos <- attributes(coefs)

  coefs <- trimws(coefs)
  coefs[] <- gsub("-","&minus;",coefs[],fixed=TRUE)
  if(split.dec){
    coefs <- t(apply(coefs,1,spltDec))
  }
  
  row.vars <- infos$row.vars[-x$kill.col]
  col.vars <- infos$col.vars[-x$kill.header]
  
  coeftitles <- matrix("",nrow=nrow(coefs),ncol=length(row.vars))
  for(i in 1:length(row.vars)){
    rv <- row.vars[[i]]
    m <- length(rv)
    n <- nrow(coefs)%/%m
    j <- 0:(m-1)*n+1
    coeftitles[j,i] <- rv
  }

  coeftitles[] <- mk_td(coeftitles, style=proc_style(upd_vect(style,firstcol)))
  
  if(split.dec)
    coefs[] <- mk_td_spltDec(coefs, style=proc_style(style))
  else 
    coefs[] <- mk_td(coefs, style=proc_style(upd_vect(style,lrpad)))
  
  body <- cbind(coeftitles,coefs)
  body <- apply(body,1,paste0,collapse="")
  body <- mk_tr(body)
  
  summaries <- x$summaries
  if(length(summaries)){
    
    nms.smrs <- rownames(summaries)
    tmp.smrs <- summaries
    
    ncc <- ncol(coefs)
    if(split.dec) ncc <- ncc%/%3
    
    summaries <- matrix("",
                        nrow=nrow(tmp.smrs),
                        ncol=ncc)
    m <- ncol(tmp.smrs)
    n <- ncc%/%m
    i <- 0:(m-1)*n+1
    summaries[,i] <- tmp.smrs
    if(split.dec){
      summaries <- t(apply(summaries,1,spltDec))
    }
    
  
    smsr.titles <- matrix("",nrow=nrow(summaries),
                             ncol=ncol(coeftitles))
    smsr.titles[,1] <- nms.smrs
    n <- nrow(summaries)
    
    smsr.titles[1,] <- mk_td(smsr.titles[1,],style=proc_style(upd_vect(style,firstcol,midrule_above)))
    smsr.titles[-c(1,n),] <- mk_td(smsr.titles[-c(1,n),],style=proc_style(upd_vect(style,firstcol)))
    smsr.titles[n,] <- mk_td(smsr.titles[n,],style=proc_style(upd_vect(style,firstcol,bottomrule)))

    if(split.dec){
      
      summaries[1,] <- mk_td_spltDec(summaries[1,],style=proc_style(upd_vect(style,midrule_above)))
      summaries[-c(1,n),] <- mk_td_spltDec(summaries[-c(1,n),],style=proc_style(style))
      summaries[n,] <- mk_td_spltDec(summaries[n,],style=proc_style(upd_vect(style,bottomrule)))
    } else {
      summaries[1,] <- mk_td(summaries[1,],style=proc_style(upd_vect(style,midrule_above)))
      summaries[-c(1,n),] <- mk_td(summaries[-c(1,n),],style=proc_style(style))
      summaries[n,] <- mk_td(summaries[n,],style=proc_style(upd_vect(style,bottomrule)))
    }
    sbody <- cbind(smsr.titles,summaries)
    sbody <- apply(sbody,1,paste0,collapse="")
    sbody <- mk_tr(sbody)
  } else sbody <- NULL
  
  header <- list()
  mm <- 1
  ncc <- ncol(coefs)
  
  for(i in 1:length(col.vars)){
    
    cv <- col.vars[[i]]
    ncv <- length(cv)
    
    cv <- rep(cv,mm)
    mm <- mm*ncv
    
    colspan <- ncc%/%mm
    
    attribs <- list(colspan=colspan)
    
    hstyle <- upd_vect(style,align.center,lrpad)
    if(i == 1)
      hstyle <- upd_vect(hstyle,toprule)
    
    hstyle1 <- upd_vect(hstyle,midrule)
    
    if(i == length(col.vars))
      hstyle <- upd_vect(hstyle,midrule)
    
    attribs$style <- proc_style(hstyle1)
    
    htmp1 <- mk_td(rep("",ncol(coeftitles)),style=proc_style(hstyle))
    htmp2 <- mk_td(cv,attribs=attribs)
    
    header <- c(header,list(c(htmp1,htmp2)))
  }
  
  header <- sapply(header,paste0,collapse="")
  header <- mk_tr(header)
  
  ans <- c("<table class=\"mtable\" style=\"border-collapse: collapse;\">",
           header,
           body,
           sbody,
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
