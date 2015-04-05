mtable_format_html <- function(x,
                               interaction.sep = " &times; ",
                               toprule=2,midrule=1,bottomrule=2,
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
  
  body <- cbind(coeftitles,coefs)
  body[] <- mk_td(body[])
  body <- apply(body,1,paste0,collapse="")
  body <- mk_tr(body)
  
  summaries <- x$summaries
  if(length(summaries)){
    
    nms.smrs <- rownames(summaries)
    tmp.smrs <- summaries
    summaries <- matrix("",
                        nrow=nrow(tmp.smrs),
                        ncol=ncol(coefs))
    m <- ncol(tmp.smrs)
    n <- ncol(coefs)%/%m
    i <- 0:(m-1)*n+1
    summaries[,i] <- tmp.smrs
    
    smsr.titles <- matrix("",nrow=nrow(summaries),
                             ncol=ncol(coeftitles))
    smsr.titles[,1] <- nms.smrs
    sbody <- cbind(smsr.titles,summaries)
    
    n <- nrow(sbody)
    
    sbody[1,] <- mk_td(sbody[1,],style=paste0("border-top: ",midrule,"px solid;"))
    sbody[-c(1,n),] <- mk_td(sbody[-c(1,n),])
    sbody[n,] <- mk_td(sbody[n,],style=paste0("border-bottom: ",bottomrule,"px solid;"))
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
      style <- paste0(style,"border-top: ",toprule,"px solid;")
    if(i == length(col.vars))
      style <- paste0(style,"border-bottom: ",midrule,"px solid;")
    if(nzchar(style))
      attribs$style <- style
    
    htmp1 <- mk_td(rep("",ncol(coeftitles)),attribs=attribs["style"])
    htmp2 <- mk_td(cv,attribs=attribs)
    header[[i]] <- c(htmp1,htmp2)
  }
  header <- sapply(header,paste0,collapse="")
  header <- mk_tr(header)
  
  ans <- c("<table>",
           header,
           body,
           sbody,
           "</table>")
  
  ans <- paste0(ans,"\n")
  return(ans)
}


mk_elem <- function(x,type,attribs=list(),...,linebreaks=FALSE,indent=0){
  start_tag <- paste0("<",type)
  
  attribs <- c(attribs,list(...))
  if(length(attribs)){
    for(n in names(attribs)){
      attrib <- paste0("\"",attribs[[n]],"\"")
      start_tag <- paste0(start_tag," ",n,"=",attrib)
    }
  }
  start_tag <- paste0(start_tag,">")
  end_tag <- paste0("</",type,">")
  
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
mk_tr <- function(x) mk_elem(x,type="tr",linebreaks=FALSE)

show_html <- function(x){
  tf <- tempfile()
  tf <- paste0(tf,".html")
  cat(mtable_format_html(x),file=tf)
  file.show(tf)
  file.remove(tf)
}