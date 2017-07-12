toLatex.data.frame <- function(object,
                           digits=getOption("digits"),
                           format="f",
                           useDcolumn=getOption("useDcolumn",TRUE),
                           numeric.colspec=if(useDcolumn)
                                               paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
                                           else "r",
                           factor.colspec="l",
                           LaTeXdec=".",
                           ddigits=digits,
                           useBooktabs=getOption("useBooktabs",TRUE),
                           toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
                           midrule=if(useBooktabs) "\\midrule" else "\\hline",
                           cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
                           bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
                           row.names=is.character(attr(object,"row.names")),
                           NAas="",
                           ...){
  n <- nrow(object)
  m <- ncol(object)
  d <- digits
  is.num <- sapply(object,is.numeric)
  is.mat <- sapply(object,is.matrix)
  m.num <- sum(is.num)
  digits <- integer(m.num)
  digits[] <- d
  fdigits <- integer(m)
  fdigits[is.num] <- digits
  fo <- format
  format <- character(m)
  format[is.num] <- fo

  body <- list()
  for(i in 1:m) {
      if(is.numeric(object[,i]))
          body.i <- formatC(object[[i]],digits=fdigits[i],format=format[i])
      else
        body.i <- as.character(object[[i]])
      body.i[is.na(body.i)] <- NAas
      body[[i]] <- format(body.i,justify="right")
  }
  body <- do.call(cbind,body)
  ans <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
  
  if(row.names){
    ans <- cbind(format(rownames(object),justify="right"),ans)
  }

  colspan <- sapply(object,NCOL)
  header <- paste0("\\multicolumn{",colspan,"}{c}{",colnames(object),"}")
  
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
  body.spec <- rep(body.spec,colspan)

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
