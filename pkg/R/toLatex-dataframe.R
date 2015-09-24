toLatex.data.frame <- function(object,
                           digits=getOption("digits"),
                           format="f",
                           useDcolumn=TRUE,
                           numeric.colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "l",
                           factor.colspec="l",
                           LaTeXdec=".",
                           ddigits=digits,
                           useBooktabs=TRUE,
                           toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
                           midrule=if(useBooktabs) "\\midrule" else "\\hline",
                           cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
                           bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
                           row.names=TRUE,
                           ...){
  n <- nrow(object)
  m <- ncol(object)
  d <- digits
  is.num <- sapply(object,is.numeric)
  m.num <- sum(is.num)
  digits <- integer(m.num)
  digits[] <- d
  fdigits <- integer(m)
  fdigits[is.num] <- digits
  fo <- format
  format <- character(m)
  format[is.num] <- fo
  #print(digits)
  body <- array("",dim=dim(object))
  for(i in 1:m) {
    if(is.numeric(object[,i]))
      body[,i] <- formatC(object[,i],digits=fdigits[i],format=format[i])
    else
      body[,i] <- as.character(object[,i])
    body[is.na(object)] <- ""
  }
  ans <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
  
  if(row.names){
    ans <- cbind(rownames(object),ans)
  }
  
  header <- sapply(colnames(object),function(x)paste("\\multicolumn{1}{c}{",x,"}",sep=""))
  header <- paste(header,collapse=" & ")
  if(row.names) header <- paste("&",header)
  header <- paste(header,"\\\\")


  ans <- apply(ans,1,paste,collapse=" & ")
  ans <- paste(ans,"\\\\")
  ans <- c(
      toprule,
      header,
      midrule,
      ans,
      bottomrule
    )

  body.spec <- rep(factor.colspec,m)
  dd <- integer(m.num)
  dd[] <- ddigits
  ddigits <- dd
  body.spec[is.num] <- numeric.colspec
  if(row.names)
    tabspec <- c("l",body.spec)
  else
    tabspec <- body.spec
  tabspec <- paste(tabspec,collapse="")
  tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
  tabend <- "\\end{tabular}"
  ans <- c(tabbegin,ans,tabend)
  structure(ans,class="Latex")
}

toLatex.data.set <- function(object,...){
  frame <- structure(object@.Data,row.names=object@row_names,names=object@names,class="data.frame")
  for(i in 1:ncol(frame))
    frame[[i]] <- format(frame[[i]])
  toLatex(frame,...)
}
