mtable_format_html <- function(x,
                               interaction.sep = " &times; ",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               ...
){
  
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
  col.vars <- infos$col.vars[-x$kill.col]
  
  coeftitles <- matrix("",nrow=nrow(coefs),ncol=length(row.vars))
  for(i in 1:length(row.vars)){
    rv <- row.vars[[i]]
    m <- length(rv)
    n <- nrow(coefs)%/%m
    j <- 0:(m-1)*n+1
    coeftitles[j,i] <- rv
  }

  coeftitles[] <- mk_td(coeftitles, style="padding-left: 0.3em;")
  
  if(split.dec)
    coefs[] <- mk_td_spltDec(coefs)
  else 
    coefs[] <- mk_td(coefs)
  
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
    
    smsr.titles[1,] <- mk_td(smsr.titles[1,],style=paste0("border-top: ",midrule,"px solid; padding-left: 0.3em;"))
    smsr.titles[-c(1,n),] <- mk_td(smsr.titles[-c(1,n),],style="padding-left: 0.3em;")
    smsr.titles[n,] <- mk_td(smsr.titles[n,],style=paste0("border-bottom: ",bottomrule,"px solid; padding-left: 0.3em;"))

    if(split.dec){
      
      summaries[1,] <- mk_td_spltDec(summaries[1,],style=paste0("border-top: ",midrule,"px solid;"))
      summaries[-c(1,n),] <- mk_td_spltDec(summaries[-c(1,n),])
      summaries[n,] <- mk_td_spltDec(summaries[n,],style=paste0("border-bottom: ",bottomrule,"px solid;"))
    } else {
      summaries[1,] <- mk_td(summaries[1,],style=paste0("border-top: ",midrule,"px solid;"))
      summaries[-c(1,n),] <- mk_td(summaries[-c(1,n),])
      summaries[n,] <- mk_td(summaries[n,],style=paste0("border-bottom: ",bottomrule,"px solid;"))
    }
    
    sbody <- cbind(smsr.titles,summaries)
    sbody <- apply(sbody,1,paste0,collapse="")
    sbody <- mk_tr(sbody)
  } else sbody <- NULL
  
  header <- list()
  for(i in 1:length(col.vars)){
    cv <- col.vars[[i]]
    m <- length(cv)
    n <- ncol(coefs)%/%m
    attribs <- list()
    attribs$colspan <- n
    style <- ""
    if(i == 1)
      style <- paste0(style,"border-top: ",toprule,"px solid; text-align: center;")
    if(i == length(col.vars))
      style <- paste0(style,"border-bottom: ",midrule,"px solid; text-align: center;")
    if(nzchar(style))
      attribs$style <- style
    
    htmp1 <- mk_td(rep("",ncol(coeftitles)),attribs=attribs["style"])
    htmp2 <- mk_td(cv,attribs=attribs)
    header[[i]] <- c(htmp1,htmp2)
  }
  header <- sapply(header,paste0,collapse="")
  header <- mk_tr(header)
  
  ans <- c("<table style=\"border-collapse: collapse;\">",
           header,
           body,
           sbody,
           "</table>")
  
  ans <- paste0(ans,collapse="\n")
  return(ans)
}

format_html.mtable <- function(x,...)
  mtable_format_html(x,...)
