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

  coeftitles[] <- mk_td(coeftitles)
  
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
    
    smsr.titles[1,] <- mk_td(smsr.titles[1,],style=paste0("border-top: ",midrule,"px solid;"))
    smsr.titles[-c(1,n),] <- mk_td(smsr.titles[-c(1,n),])
    smsr.titles[n,] <- mk_td(smsr.titles[n,],style=paste0("border-bottom: ",bottomrule,"px solid;"))

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
  
  ans <- paste0(ans,"\n")
  return(ans)
}


mk_elem <- function(x,type,extra="",attribs=list(),...,linebreaks=FALSE,indent=0){
  start_tag <- paste0("<",type)
  if(nzchar(extra))
    start_tag <- paste(start_tag,extra)
  end_tag <- paste0("</",type,">")
  
  if(!missing(x)) 
    start_tag <- rep(start_tag,length(x))
  
  attribs <- c(attribs,list(...))
  if(length(attribs)){
    for(n in names(attribs)){
      attrib <- character(length(start_tag))
      attrib[] <- paste0("\"",attribs[[n]],"\"")
      use <- nzchar(attrib)
      start_tag[use] <- paste0(start_tag[use]," ",n,"=",attrib[use])
    }
  }
  start_tag <- paste0(start_tag,">")
  
  if(linebreaks && indent>0)
    indent <- paste0(rep(" ",indent),collapse="")
  else indent <- ""  
  
  if(!missing(x)){
    if(linebreaks){
      x <- paste0(indent,"   ",x,"\n")
      start_tag <- paste0(indent,start_tag,"\n")
      end_tag <- paste0(indent,end_tag,"\n")
    }
    res <- paste0(start_tag,x,end_tag)
  }
  else {
    res <- start_tag
    if(linebreaks)
      res <- paste0(indent,res,"\n")
  }
  
  return(res)
}
 
mk_td <- function(x,...)mk_elem(x,type="td",...,linebreaks=FALSE)
mk_th <- function(x,...)mk_elem(x,type="th",...,linebreaks=FALSE)
mk_tr <- function(x,...) mk_elem(x,type="tr",...,linebreaks=FALSE)

show_html <- function(x,...){
  tf <- tempfile()
  tf <- paste0(tf,".html")
  cat(mtable_format_html(x,...),file=tf)
  file.show(tf)
  file.remove(tf)
}

spltDec <- function(x,at="."){
  y <- strsplit(x,at,fixed=TRUE)
  y1 <- sapply(y,"[",1)
  y3 <- sapply(y,"[",2)
  y1[is.na(y1)] <- ""
  y3[is.na(y3)] <- ""
  y2 <- ifelse(grepl("[[:digit:]]+",y3),at,"")
  y <- rbind(y1,y2,y3)
  as.vector(y)
}

mk_td_spltDec <- function(x,style=""){
  
  if(!is.matrix(x))
    x <- t(as.matrix(x))
  
  tmp <- as.vector(t(x))
  
  stl <- c("text-align: right; margin-right: 0px; padding-right: 0px; padding-left: 0.3em;",
           "text-align: center; margin-left: 0px; padding-left: 0px; margin-right: 0px; padding-right: 0px; width: 1px;",
           "text-align: left; margin-left: 0px; padding-left: 0px; padding-right: 0.3em;")
  if(nzchar(style))
    style <- paste(style,stl)
  else
    style <- stl
  
  tmp <- mk_td(tmp,
                style=style)
  matrix(tmp,
         nrow=nrow(x),
         ncol=ncol(x),
         byrow=TRUE)
}