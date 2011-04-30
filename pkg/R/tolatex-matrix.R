toLatex.matrix <- function(object,
          show.titles=TRUE,
          show.vars=FALSE,
          show.xvar=show.vars,
          show.yvar=show.vars,
          digits=if(is.table(object)) 0 else getOption("digits"),
          format="f",
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=digits,
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          ...){
  n <- nrow(object)
  m <- ncol(object)
  d <- digits
  digits <- integer(m)
  digits[] <- d
  fo <- format
  format <- integer(m)
  format[] <- fo
  #print(digits)
  body <- array("",dim=dim(object))
  for(i in seq(along=digits)) {
    #print(digits[i])
    body[,i] <- formatC(object[,i],digits=digits[i],format=format[i])
    body[is.na(object)] <- ""
    }
  ans <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
  if(show.titles){
    if(length(rownames(object))){
      ans <- cbind(rownames(object),ans)
    }
    if(length(colnames(object))){
      header <- sapply(colnames(object),function(x)paste("\\multicolumn{1}{c}{",x,"}",sep=""))
      if(!show.yvar || !length(names(dimnames(object)))){
        if(length(rownames(object))){
          if(show.xvar && length(names(dimnames(object))))
              header <- c(names(dimnames(object))[1],header)
          else header <- c("",header)
        }
        header <- paste(header,collapse=" & ")
        header <- paste(header,"\\\\")
      }
      else {
        super.header <- paste("\\multicolumn{",m,"}{c}{",names(dimnames(object))[2],"}",sep="")
        if(length(rownames(object))){
          if(show.xvar && length(names(dimnames(object))))
              super.header <- c(names(dimnames(object))[1],super.header)
          else super.header <- c("",super.header)
          header <- c("",header)
          if(length(cmidrule))
            cmidrule <- paste(cmidrule,"{",2,"-",m+1,"}",sep="")
        }
        else if(length(cmidrule))
          cmidrule <- paste(cmidrule,"{",1,"-",m,"}",sep="")
        header <- paste(header,collapse=" & ")
        header <- paste(header,"\\\\")
        super.header <- paste(super.header,collapse=" & ")
        super.header <- paste(super.header,"\\\\")
        header <- c(super.header,cmidrule,header)
      }
    }
  }
  ans <- apply(ans,1,paste,collapse=" & ")
  ans <- paste(ans,"\\\\")
  if(show.titles && length(colnames(object)))
    ans <- c(
        toprule,
        header,
        midrule,
        ans,
        bottomrule
        )
  else {
    ans <- c(
        toprule,
        ans,
        bottomrule
        )
  }
  body.spec <- character(ncol(object))
  body.spec[] <- colspec
  if(show.titles && length(rownames(object)))
    tabspec <- c("l",body.spec)
  else
    tabspec <- body.spec
  tabspec <- paste(tabspec,collapse="")
  tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
  tabend <- "\\end{tabular}"
  ans <- c(tabbegin,ans,tabend)
  structure(ans,class="Latex")
}

toLatex.default <- function(object,...) toLatex.matrix(as.matrix(object),...)