str.has <- function(text,has,not=NULL,how=c("all","any")){
    how <- match.fun(match.arg(how))

    hasit <- sapply(has,function(pat)regexpr(pat,text,fixed=TRUE) > 0)
    if(is.matrix(hasit))
        hasit <- apply(hasit,1,how)
    else
        hasit <- all(hasit)


    if(!length(not)) return(hasit)
    # else
    hasnot <- sapply(not,function(pat)regexpr(pat,text,fixed=TRUE) > 0)
    if(is.matrix(hasnot))
        hasnot <- apply(hasnot,1,how)
    else
        hasnot <- all(hasnot)

    hasit & !hasnot
}



setCoefTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  CoefTemplates <- get("CoefTemplates", env=.memiscEnv)
  OldCoefTemplates <- CoefTemplates
    for(coef.style in argnames){
      CoefTemplates[[coef.style]] <- args[[coef.style]]
  }
  assign("CoefTemplates",CoefTemplates, env=.memiscEnv)
  return(invisible(OldCoefTemplates))
}

getFirstMatch <- function(x,n){
  for(n. in n){
    if(n. %in% names(x)) return(x[[n.]])
  }
  return(x[["default"]])
}

getCoefTemplate <- function(style){
  CoefTemplates <- get("CoefTemplates", env=.memiscEnv)
  if(missing(style)) return(CoefTemplates)
  else return(CoefTemplates[[style]])
}


getSummary <- function(obj,alpha=.05,...) UseMethod("getSummary")

getSummary.lm <- function(obj,
            alpha=.05,
            ...
            ){
  smry <- summary(obj)
  coef <- smry$coef

  numdf <- unname(smry$fstatistic[2])
  dendf <- unname(smry$fstatistic[3])

  lower <- coef[,1] + coef[,2]*qt(p=alpha/2,df=dendf)
  upper <- coef[,1] + coef[,2]*qt(p=1-alpha/2,df=dendf)

  coef <- cbind(coef,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  sigma <- smry$sigma
  r.squared <- smry$r.squared
  adj.r.squared <- smry$adj.r.squared
  F <- unname(smry$fstatistic[1])
  p <- pf(F,numdf,dendf,lower.tail=FALSE)
  N <- sum(smry$df[1:2])
  ll <- logLik(obj)
  deviance <- deviance(obj)
  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          sigma         = sigma,
          r.squared     = r.squared,
          adj.r.squared = adj.r.squared,
          F             = F,
          numdf         = numdf,
          dendf         = dendf,
          p             = p,
          logLik        = ll,
          deviance      = deviance,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=obj$xlevels,call=obj$call)
}


getSummary.glm <- function(obj,
            alpha=.05,
            ...){

  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
    else sum(smry$df[1:2])

  coef <- smry$coef

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  phi <- smry$dispersion
  LR <- smry$null.deviance - smry$deviance
  df <- smry$df.null - smry$df.residual

  ll <- logLik(obj)
  deviance <- deviance(obj)


  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(-smry$null.deviance/N)
    #LM.pwr <- exp(-smry$deviance/N)

    McFadden <- 1- smry$deviance/smry$null.deviance
    Cox.Snell <- 1 - exp(-LR/N)
    Nagelkerke <- Cox.Snell/(1-L0.pwr)
    }
  else {
    LR <- NA
    df <- NA
    p <- NA
    McFadden <- NA
    Cox.Snell <- NA
    Nagelkerke <- NA
    }

  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          phi         = phi,
          LR             = LR,
          df         = df,
          p             = p,
          logLik        = ll,
          deviance      = deviance,
          McFadden      = McFadden,
          Cox.Snell       = Cox.Snell,
          Nagelkerke    = Nagelkerke,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=obj$xlevels,call=obj$call)
}

getSummaryTemplate <- function(x){
  SummaryTemplates <- get("SummaryTemplates", env=.memiscEnv)
  if(missing(x)) return(SummaryTemplates)
  if(is.character(x)) cls <- x
  else cls <- class(x)
  return(getFirstMatch(SummaryTemplates,cls))
}

setSummaryTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  OldSummaryTemplates <- SummaryTemplates <- get("SummaryTemplates", env=.memiscEnv)
  for(cls in argnames){
      SummaryTemplates[[cls]] <- args[[cls]]
  }
  assign("SummaryTemplates",SummaryTemplates,env=.memiscEnv)
  return(invisible(OldSummaryTemplates))
}

prettyNames <- function(coefnames,
                        contrasts,
                        xlevels,
                        factor.style=getOption("factor.style"),
                        baselevel.sep=getOption("baselevel.sep")
                        ){
    termorders <- sapply(strsplit(coefnames,":",fixed=TRUE),length)
    ordergroups <- split(coefnames,termorders)
    ordergroups <- lapply(ordergroups,prettyNames1,
                        contrasts=contrasts,
                        xlevels=xlevels,
                        factor.style=factor.style,
                        baselevel.sep=baselevel.sep)
    unsplit(ordergroups,termorders)
}

prettyNames1 <- function(str,
                        contrasts,
                        xlevels,
                        factor.style=getOption("factor.style"),
                        baselevel.sep=getOption("baselevel.sep")
                        ){
   str <- gsub(":"," x ",str,fixed=TRUE)
   for(f in names(contrasts)){
      contrast.f <- contrasts[[f]]
      levels <- xlevels[[f]]
      #if(!length(levels)) levels <- c("FALSE","TRUE")
      if(!length(levels)) {
        str <- gsub(paste(f,"TRUE",sep=""),f,str,fixed=TRUE)
        next
      }
      if(is.character(contrast.f))
        contrast.matrix <- do.call(contrast.f,list(n=levels))
      else if(is.matrix(contrast.f))
        contrast.matrix <- contrast.f
      levels.present <- sapply(levels,function(level)
            any(str.has(str,c(f,level)))
            )
      if(all(levels.present))
        oldlabels <- newlabels <- levels
      else if(!length(colnames(contrast.matrix))){
        oldlabels <- newlabels <- as.character(1:ncol(contrast.matrix))
        }
      else if(is.character(contrast.f) &&
          contrast.f %in% c(
              "contr.treatment",
              "contr.SAS"
              )){
         baselevel <- setdiff(rownames(contrast.matrix),colnames(contrast.matrix))
         newlabels <- paste(colnames(contrast.matrix),baselevel,sep=baselevel.sep)
         oldlabels <- colnames(contrast.matrix)
      }
      else if(is.character(contrast.f) &&
          contrast.f %in% c(
              "contr.sum",
              "contr.helmert"
              )){
         newlabels <- apply(contrast.matrix,2,
                                          function(x)rownames(contrast.matrix)[x>=1])
         oldlabels <- colnames(contrast.matrix)
      }
      else if(
        all(colnames(contrast.matrix) %in% rownames(contrast.matrix))
        ){
         baselevel <- setdiff(rownames(contrast.matrix),colnames(contrast.matrix))
         newlabels <- paste(colnames(contrast.matrix),baselevel,sep=baselevel.sep)
         oldlabels <- colnames(contrast.matrix)
      }
      else {
        oldlabels <- newlabels <- colnames(contrast.matrix)
      }
      from <- paste(f,oldlabels,sep="")
      to <- sapply(newlabels,
        function(l)applyTemplate(c(f=f,l=l),template=factor.style))
      for(i in 1:length(from))
        str <- gsub(from[i],to[i],str,fixed=TRUE)
   }
   str
}


prettyNames.old <- function(str,
                        contrasts,
                        xlevels,
                        factor.style=getOption("factor.style"),
                        baselevel.sep=getOption("baselevel.sep")
                        ){
   str <- gsub(":"," x ",str,fixed=TRUE)
   for(f in names(contrasts)){
      contrast.f <- contrasts[[f]]
      levels <- xlevels[[f]]
      if(is.character(contrast.f))
        contrast.matrix <- do.call(contrast.f,list(n=levels))
      if(is.matrix(contrast.f))
        contrast.matrix <- contrast.f
      if(!length(colnames(contrast.matrix))){
        oldlabels <- newlabels <- as.character(1:ncol(contrast.matrix))
        }
      else if(is.character(contrast.f) &&
          contrast.f %in% c(
              "contr.treatment",
              "contr.SAS"
              )){
         baselevel <- setdiff(rownames(contrast.matrix),colnames(contrast.matrix))
         newlabels <- paste(colnames(contrast.matrix),baselevel,sep=baselevel.sep)
         oldlabels <- colnames(contrast.matrix)
      }
      else if(is.character(contrast.f) &&
          contrast.f %in% c(
              "contr.sum",
              "contr.helmert"
              )){
         newlabels <- apply(contrast.matrix,2,
                                          function(x)rownames(contrast.matrix)[x>=1])
         oldlabels <- colnames(contrast.matrix)
      }
      else {
        oldlabels <- newlabels <- colnames(contrast.matrix)
      }
      from <- paste(f,oldlabels,sep="")
      to <- sapply(newlabels,
        function(l)applyTemplate(c(f=f,l=l),template=factor.style))
      for(i in 1:length(from))
        str <- gsub(from[i],to[i],str,fixed=TRUE)
   }
   str
}

bind_arrays <- function(args,along=1){
  along.dn <- unlist(lapply(args,function(x)dimnames(x)[[along]]))
  groups <- sapply(args,function(x)dim(x)[along])
  dn <- dimnames(args[[1]])
  keep.dn <- dn[-along]
  dim1 <- dim(args[[1]])
  keep.dim <- dim1[-along]
  ldim <- length(dim1)
  dimseq <- seq_len(ldim)
  perm.to <- dimseq
  perm.to[ldim] <- along
  perm.to[along] <- ldim
  res <- lapply(args,function(x){
    x <- aperm(x,perm.to)
    dim(x) <- c(prod(dim(x)[-ldim]),dim(x)[ldim])
    x
    })
  res <- do.call(cbind,res)
  dim(res) <- c(keep.dim,ncol(res))
  dimnames(res) <- c(keep.dn,list(along.dn))
  structure(aperm(res,perm.to),groups=groups)
}

mtable <- function(...,
                    coef.style=getOption("coef.style"),
                    summary.stats=TRUE,
                    factor.style=getOption("factor.style"),
                    getSummary=function(obj,...)UseMethod("getSummary"),
                    float.style=getOption("float.style"),
                    digits=min(3,getOption("digits")),
                    drop=TRUE
                    ){
  args <- list(...)
  if(length(args)==1 && inherits(args[[1]],"by"))
    args <- args[[1]]
  argnames <- names(args)
  if(!length(argnames)) {
    m<-match.call(expand.dots=FALSE)
    argnames <- sapply(m$...,paste)
  }
  n.args <- length(args)

  arg.classes <- lapply(args,class)
  if(any(sapply(arg.classes,length))==0) stop("don\'t know how to handle these arguments")
  summaries <- lapply(args,getSummary)
  calls <- lapply(summaries,function(x)x$call)
  names(calls) <- argnames

  ctemplate <- as.matrix(getCoefTemplate(coef.style))
  ctdims <- dim(ctemplate)
  lctdims <- length(ctdims)
  if(lctdims>2) stop("can\'t handle templates with dim>2")
  getCoef1 <- function(coef,contrasts,xlevels){
        dimnames(coef)[[1]] <- prettyNames(dimnames(coef)[[1]],
                        contrast=contrasts,
                        xlevels=xlevels,
                        factor.style=factor.style)
        adims <- if(length(dim(coef))==2) 1 else c(1,3)
        ans <- apply(coef,adims,function(x)applyTemplate(x,
            template=ctemplate,float.style=float.style,digits=digits))
        if(length(dim(ctemplate))){
          newdims <- c(dim(ctemplate),dim(ans)[-1])
          newdimnames <- c(dimnames(ctemplate),dimnames(ans)[-1])
          newdimnames <- lapply(1:length(newdims),function(i){
              if(length(newdimnames[[i]])) return(newdimnames[[i]])
              else return(as.character(1:newdims[i]))
              })
          dim(ans) <- newdims
          dimnames(ans) <- newdimnames
        } else rownames(ans) <- names(ctemplate)
        ans[ans=="()"] <- ""
        return(ans)
  }
  getCoef <- function(i){
        coef.i <- summaries[[i]]$coef
        contrasts.i <- summaries[[i]]$contrasts
        xlevels.i <- summaries[[i]]$xlevels
        if(is.list(coef.i))
          lapply(coef.i,getCoef1,contrasts=contrasts.i,xlevels=xlevels.i)
        else
          getCoef1(coef.i,contrasts=contrasts.i,xlevels=xlevels.i)
      }

  coefs <- lapply(seq(n.args),getCoef)
  isList <- sapply(coefs,is.list)
  if(any(isList)){
    all.names <- unique(unlist(lapply(coefs,names)))
    coefs <- lapply(coefs,function(x){
      if(is.list(x)) x else structure(list(x),names=all.names[1])
    })
    coefs.tmp <- vector(length=length(all.names),mode="list")
    names(coefs.tmp) <- all.names
    for(n in all.names){
      tmp <- lapply(coefs,.subset2,n)
      isNULL <- sapply(tmp,is.null)
      if(any(isNULL)){
        firstNonNULL <- tmp[[min(which(!isNULL))]]
        dummy <- array(NA,dim=dim(firstNonNULL),dimnames=dimnames(firstNonNULL))
        tmp[isNULL] <- list(dummy)
      }
      tmp <- clct.arrays(tmp)
      n.dims <- length(dim(tmp))
      dimnames(tmp)[[n.dims]] <- argnames
      tmp[is.na(tmp)] <- ""
      coefs.tmp[[n]] <- tmp
    }
    coefs <- bind_arrays(coefs.tmp,3)
  }
  else{
    coefs <- clct.arrays(coefs)
    n.dims <- length(dim(coefs))
    dimnames(coefs)[[n.dims]] <- argnames
    coefs[is.na(coefs)] <- ""
  }
  groups <- attr(coefs,"groups")

  n.dims <- length(dim(coefs))

  dimnames(coefs)[[n.dims]] <- argnames

  if(drop && length(dim(coefs))>3 ){
    cdims <- dim(coefs)
    ckeep <- cdims > 1 | 1:length(dim(coefs)) <= 3
    dn <- dimnames(coefs)
    dim(coefs) <- dim(coefs)[ckeep]
    dimnames(coefs) <- dn[ckeep]
    dims <- sum(ckeep)
  }
  as.row <- c(1,3)
  as.col <- which(!(seq(length(dim(coefs))) %in% as.row))
  kill.col <- 2
  kill.header <- length(as.col)
  coef.dim <- 3

  if(isTRUE(summary.stats) || is.character(summary.stats) && length(summary.stats)) {
    stemplates <- lapply(args,getSummaryTemplate)
    sumstats <- lapply(seq(n.args),function(i){
          drop(applyTemplate(summaries[[i]]$sumstat,
              template=stemplates[[i]],digits=digits))
        })
    sumstats <- clct.vectors(sumstats)
    colnames(sumstats) <- argnames
    if(is.character(summary.stats) && !all(summary.stats %in% rownames(sumstats))){
      undefnd <- summary.stats[!(summary.stats %in% rownames(sumstats))]
      undefnd <- paste(sQuote(undefnd),sep=", ")
      if(length(undefnd)==1)
        stop("summary statistic ",undefnd," is undefined")
      else
        stop("summary statistics ",undefnd," are undefined")
    }
    sumstats <- sumstats[summary.stats,,drop=FALSE]
    sumstats[is.na(sumstats)] <- ""
    substats <- as.table(sumstats)
  }
  else sumstats <- NULL

  structure(list(
    coefficients=as.table(coefs),
    groups=groups,
    summaries=sumstats,
    calls=calls,
    as.row=as.row,
    as.col=as.col,
    kill.col=kill.col,
    kill.header=kill.header,
    coef.dim=coef.dim),
    class="mtable")
}






centerAt <- function(x,at=getOption("OutDec"),integers=c("dot","right","left"),skip=0){
  has.dot <- setdiff(grep(at,x,fixed=TRUE),skip)
  if(!any(has.dot>0)) return(x)
  x <- trimws(x)
  is.int <- setdiff(seq(x),union(has.dot,skip))
  splitted <- strsplit(x[has.dot],at,fixed=TRUE)
  left <- sapply(splitted,function(x)x[1])
  maxleft <- max(nchar(left))
  right <- sapply(splitted,function(x)paste(x[-1],collapse="."))
  maxright <- max(nchar(right))
  maxcentered <- maxleft+maxright+1

  if(any(is.int>0)){
    integers <- match.arg(integers)
    if(integers=="right"){
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="right",
                          width=maxcentered)
    }
    if(integers=="left"){
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="left",
                          width=maxcentered)
    }
    if(integers=="dot"){
      maxleft <- max(maxleft,max(nchar(as.character(x[is.int]))))
      left <- format(left,justify="right",width=maxleft)
      right <- format(right,justify="left",width=maxright)
      fintegers <- format(x[is.int],
                          justify="right",
                          width=maxleft)
      fintegers <- paste(fintegers,format(" ",width=maxright))
    }
    centered <- paste(left,right,sep=".")
    maxcentered <- max(nchar(centered))
    x[has.dot] <- centered
    x[is.int] <- fintegers
  } else {
    left <- format(left,justify="right",width=maxleft)
    right <- format(right,justify="left",width=maxright)
    centered <- paste(left,right,sep=".")
    x[has.dot] <- centered
  }
  if(any(as.logical(skip)))
    x[skip] <- format(x[skip],width=maxcentered,justify="centre")

  return(x)
}




format.mtable <- function(x,
          coef.title="Coefficients",
          summary.title="Summaries",
          colsep="\t",
          rowsep="\n",
          trim = TRUE,
          trimleft=trim,
          trimright= trim,
          center.at=NULL,
          align.integers=c("dot","right","left"),
          topsep="",
          bottomsep="",
          sectionsep="",
          compact=TRUE,
          forLaTeX=FALSE,
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=getOption("digits"),
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = if(forLaTeX) " $\\times$ " else " x ",
          center.summaries=FALSE,
          ...
          ){
  if(forLaTeX) compact <- FALSE

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
  summaries <- x$summaries
  if(compact){
    ans <- trimws(coefs)
    col.vars <- rev(infos$col.vars)
    ans <- coefs
    for(i in 1:length(col.vars)){
      header <- character(NCOL(ans))
      cv <- col.vars[[i]]
      lcv <- length(cv)
      header[] <- cv
      ans <- rbind(header,ans)
      if(length(summaries)){
        if(i == length(col.vars)){
          if(ncol(ans)>ncol(summaries)){
              tmp <- summaries
              summaries <- matrix("",nrow=nrow(tmp),ncol=ncol(ans))
              summaries[,1] <- tmp
              rownames(summaries) <- rownames(tmp)
          }
          ans <- rbind(ans,summaries)
        }
      }
      dim(ans) <- c(nrow(ans),lcv,ncol(ans)/lcv)
      ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=colsep)))
    }
    row.vars <- infos$row.vars[-x$kill.col]
    leaders <- character(NROW(ans))
    for(i in 1:length(row.vars)){
      tmp <- matrix("",nrow=length(row.vars[[i]]),
                        ncol=nrow(coefs)/length(row.vars[[i]]))
      tmp[,1] <- row.vars[[i]]
      tmp <- c(rep("",length(col.vars)),t(tmp))
      if(i == 1) tmp <- c(tmp,rownames(summaries))
      else tmp <- c(tmp,rep("",nrow(summaries)))
      if(i == length(row.vars))
        leaders <- as.matrix(paste(leaders,tmp,sep=""))
      else
        leaders <- as.matrix(paste(leaders,tmp,colsep,sep=""))
    }
    ans <- paste(leaders,ans,sep=colsep)
    ans <- ans[-x$kill.header]
    ans <- paste(ans,rowsep,sep="")
    return(ans)
  }
  else if(!forLaTeX){
    if(length(center.at)){
      align.integers <- match.arg(align.integers)
      coefs <- apply(coefs,2,centerAt,
                            at=center.at,
                            integers=align.integers)
      if(length(summaries)){
        summaries <- apply(summaries,2,centerAt,
                              at=center.at,
                              integers=align.integers)
        dim(summaries) <- dim(x$summaries)
        dimnames(summaries) <- dimnames(x$summaries)
      }
    }
    else
      coefs <- trimws(coefs,left=trimleft,right=trimright)
    col.vars <- rev(infos$col.vars)
    ans <- coefs
    for(i in 1:length(col.vars)){
      header <- character(NCOL(ans))
      cv <- col.vars[[i]]
      lcv <- length(cv)
      header[] <- cv
      ans <- rbind(header,ans)
      if(length(summaries)){
        if(i == length(col.vars)){
          if(ncol(ans)>ncol(summaries)){
              tmp <- summaries
              summaries <- matrix("",nrow=nrow(tmp),ncol=ncol(ans))
              summaries[,1] <- tmp
              rownames(summaries) <- rownames(tmp)
          }
          ans <- rbind(ans,summaries)
        }
      }
      ans <- format(ans,justify="centre")
      dim(ans) <- c(nrow(ans),lcv,ncol(ans)/lcv)
      ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=colsep)))
    }
    row.vars <- infos$row.vars[-x$kill.col]
    leaders <- character(NROW(ans))
    for(i in 1:length(row.vars)){
      tmp <- matrix("",nrow=length(row.vars[[i]]),
                        ncol=nrow(coefs)/length(row.vars[[i]]))
      tmp[,1] <- row.vars[[i]]
      tmp <- c(rep("",length(col.vars)),t(tmp))
      if(length(summaries)){
        if(i == 1) tmp <- c(tmp,rownames(summaries))
        else tmp <- c(tmp,rep("",nrow(summaries)))
      }
      tmp <- format(tmp,justify="left")
      leaders <- as.matrix(paste(leaders,tmp,colsep,sep=""))
    }
    ans <- paste(leaders,ans,sep=colsep)
    headlines <- seq(length(col.vars))
    if(x$kill.header){
        ans <- ans[-x$kill.header]
        headlines <- headlines[-x$kill.header]
    }
    coeflines <- (if(length(headlines)) max(headlines) else 0)+ seq(nrow(coefs))
    if(length(summaries))
      sumrylines <- max(coeflines) + seq(nrow(summaries))
    if((any(nchar(topsep)))){
      toprule <- rep(topsep,nchar(ans[1]))
      toprule <- paste(toprule,collapse="")
    } else
    toprule <- NULL
    if((any(nchar(sectionsep)))){
      secrule <- rep(sectionsep,nchar(ans[1]))
      secrule <- paste(secrule,collapse="")
    } else
    secrule <- NULL
    if((any(nchar(bottomsep)))){
      botrule <- rep(bottomsep,nchar(ans[1]))
      botrule <- paste(botrule,collapse="")
    } else
    botrule <- NULL
    ans <- c(
            toprule,
            if(length(headlines)) ans[headlines],
            if(length(headlines)) secrule,
            ans[coeflines],
            if(length(summaries)) secrule,
            if(length(summaries)) ans[sumrylines],
            botrule
            )
    ans <- paste(paste(ans,collapse=rowsep),rowsep,sep="")
    return(ans)
  }
  else { ## forLaTeX
    if(missing(trimleft)) trimleft <- FALSE
    if(missing(trimright)) trimright <- FALSE
    if(missing(center.at)) center.at <- getOption("OutDec")
    align.integers <- match.arg(align.integers)
    col.vars <- rev(infos$col.vars)
    row.vars <- infos$row.vars[-x$kill.col]
    coefs <- apply(coefs,2,centerAt,
                          at=center.at,
                          integers=align.integers)
    coefs <- sub("(\\*+)","^{\\1}",coefs)
    coefs <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",coefs)
    if(!useDcolumn){
      tmpatt <- attributes(coefs)
      coefs <- paste("$",coefs,"$",sep="")
      attributes(coefs) <-tmpatt
      }

    if(length(summaries)){
      if(nrow(summaries)>1)
          summaries <- apply(summaries,2,centerAt,
                            at=center.at,
                            integers=align.integers)
      if(!useDcolumn){
        tmpatt <- attributes(summaries)
        summaries <- paste("$",summaries,"$",sep="")
        attributes(summaries) <-tmpatt
        }
      tmp.sumry <- array("",dim=c(nrow(summaries),ncol(coefs)/ncol(summaries),ncol(summaries)))
      if(center.summaries)
          sumpos <- (dim(tmp.sumry)[2]+1)%/%2
      else
          sumpos <- 1
      tmp.sumry[,sumpos,] <- summaries
      dim(tmp.sumry) <- c(nrow(summaries),ncol(coefs))
      ans <- rbind(coefs,tmp.sumry)
    }
    else ans <- coefs

    header <- character(1:length(col.vars))
    for(i in 1:length(col.vars)){
      tmp.header <- character(NCOL(ans))
      cv <- col.vars[[i]]
      lcv <- length(cv)
      tmp.header[] <- cv
      mcols <- ncol(coefs)/length(tmp.header)
      tmp.header <- paste("\\multicolumn{",mcols,"}{c}{",trimws(tmp.header),"}",sep="")
      if(i == length(col.vars) && length(col.vars) > 1)
        tmp.header <- paste(tmp.header,collapse=" && ")
      else
        tmp.header <- paste(tmp.header,collapse=" & ")
      if(length(col.vars)>1)
        tmp.header <- c(rep("",length(row.vars)+1),t(tmp.header))
      else
        tmp.header <- c(rep("",length(row.vars)),t(tmp.header))
      tmp.header <- paste(tmp.header,collapse="&")
      header[i] <- tmp.header

      ans <- format(ans,justify="centre")
      dim(ans) <- c(nrow(ans),lcv,ncol(ans)/lcv)
      if(i == length(col.vars) && length(col.vars) > 1)
        ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=" && ")))
      else
        ans <- as.matrix(apply(ans,c(1,3),function(x)paste(x,collapse=" & ")))
    }
    leaders <- character(NROW(coefs)+if(length(summaries)) nrow(summaries) else 0)
    for(i in 1:length(row.vars)){
      tmp <- matrix("",nrow=length(row.vars[[i]]),
                        ncol=nrow(coefs)/length(row.vars[[i]]))
      tmp[,1] <- row.vars[[i]]
      tmp <- c(t(tmp))
      if(length(summaries)){
        if(i == 1) tmp <- c(tmp,rownames(summaries))
        else tmp <- c(tmp,rep("",nrow(summaries)))
      }
      tmp <- format(tmp,justify="left")
      if(i < length(row.vars) || length(col.vars) > 1)
        leaders <- as.matrix(paste(leaders,tmp," & " ,sep=""))
      else
        leaders <- as.matrix(paste(leaders,tmp,sep=""))
    }
    ans <- paste(leaders,ans,sep=" & ")
    header <- paste(header,"\\\\",sep="")

    if(length(cmidrule) && length(col.vars)>1){
        ccol.vars <- rev(col.vars[-1])
      n.grps <- length(ccol.vars[[1]])
      len.grps <- ncol(coefs)/n.grps + 1
      strt.grps <- length(row.vars)+1 + (seq(n.grps)-1)*len.grps+1
      cmidrules <- character(length(header))
      for(i in 1:length(ccol.vars)){
        n.cmrl <- length(ccol.vars[[i]])
        len.cmrl <- ncol(coefs)/n.cmrl
        per.grp <- n.cmrl/n.grps
        strt.igrp <- (seq(per.grp)-1)*len.cmrl
        end.igrp <- seq(per.grp)*len.cmrl - 1
        strt.cmrl <- c(outer(strt.igrp,strt.grps,"+"))
        end.cmrl <- c(outer(end.igrp,strt.grps,"+"))
        ccmidrule <- paste(cmidrule,"{",strt.cmrl,"-",end.cmrl,"}",sep="")
        cmidrules[i] <- paste(ccmidrule,collapse="")
      }
      header <- cbind(rev(header),rev(cmidrules))
      if(x$kill.header)
        header <- header[-x$kill.header,,drop=FALSE]
      header <- c(t(header))
      header <- header[-length(header)]
    }
    else{
      header <- rev(header)
      if(x$kill.header)
        header <- header[-x$kill.header]
      }
    ans <- paste(ans,"\\\\",sep="")
    coeflines <- seq(nrow(coefs))
    if(length(summaries))
      sumrylines <- max(coeflines) + seq(nrow(summaries))
    ans <- c(
            toprule,
            if(length(header))header,
            if(length(header))midrule,
            ans[coeflines],
            if(length(summaries)) midrule,
            if(length(summaries)) ans[sumrylines],
            bottomrule
            )

    leader.spec <- paste(rep("l",length(row.vars)),collapse="")
    coef.spec <- character(ncol(coefs)/length(rev(col.vars)[[1]]))
    coef.spec[] <- colspec
    coef.spec <- paste(coef.spec,collapse="")
    tabspec <- c(leader.spec,rep(coef.spec,length(rev(col.vars)[[1]])))
    if(length(col.vars) > 1)
        tabspec <- paste(tabspec,collapse="c")
    else
        tabspec <- paste(tabspec,collapse="")
    tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
    tabend <- "\\end{tabular}"

    splash <- c("%")
    splash <- c(splash,"% Calls:")
    calls <- x$calls
    for(i in seq(calls)){
        tmp <- paste("% ",names(calls)[i],": ",sep="")
        tmp <- paste(tmp,paste(trimws(deparse(calls[[i]])),collapse=" "),"")
        splash <- c(splash,tmp)
      }
    splash <- c(splash,"%")
    splashrule <- rep("%",max(sapply(splash,nchar),sapply(ans,nchar)))
    splashrule <- paste(splashrule,collapse="")
    splash <- c(splashrule,splash,splashrule)
    c(splash,tabbegin,ans,tabend)
  }
}









print.mtable <- function(x,trim=FALSE,center.at=getOption("OutDec"),
      colsep=" ",
      topsep="=",bottomsep="=",sectionsep="-",...){
  calls <- x$calls
  cat("\nCalls:\n")
  for(i in seq(calls)){
      cat(names(calls)[i],": ",sep="")
      print(calls[[i]])
    }
  cat("\n")
  cat(format.mtable(x,trimleft=trim,trimright=trim,center.at=center.at,
      compact=FALSE,
      colsep=colsep,topsep=topsep,bottomsep=bottomsep,sectionsep=sectionsep,...),
      sep="")
}

toLatex.mtable <- function(object,...){
  structure(format.mtable(x=object,...,forLaTeX=TRUE),
  class="Latex")
}


write.mtable <- function(object,file="",...){
  l <- list(...)
  f <- format.mtable(object,...)
  if(isTRUE(l[["forLaTeX"]]))
    f <- paste(f,"\n",sep="")
  cat(f,file=file,sep="")
}



drop.mtable <- function(x,...){
  cdims <- dim(x$coefficients)
  ckeep <- cdims > 1
  x$coefficients <- drop(x$coefficients)

  newdims <- rep(NA,length(cdims))
  newdims[ckeep] <- seq(length(which(ckeep)))
  as.col <- newdims[x$as.col]
  as.row <- newdims[x$as.row]
  x$as.col <- as.col[is.finite(as.col)]
  x$as.row <- as.row[is.finite(as.row)]
  x
}

relabel.mtable <- function(x,...,gsub=FALSE,fixed=!gsub,warn=FALSE){
 relabelTab <- function(tab){
  n.dims <- length(dim(tab))
  for(i in 1:n.dims){
    tab <- dimrename(tab,dim=i,...,gsub=gsub,fixed=fixed,warn=warn)
    }
  tab
 }
 x$coefficients <- relabelTab(x$coefficients)
 x$summaries <- relabelTab(x$summaries)
 return(x)
}

## The following two functions are contributed by
## Christopher N. Lawrence, Ph.D. <c.n.lawrence@gmail.com>
## Assistant Professor of Political Science
## Texas A&M International University
## 313 LBVSC, 5201 University Blvd
## Laredo, Texas 78041-1920

getSummary.polr <- function(obj,
            alpha=.05,
            ...){

  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
    else smry$nobs

  coef <- smry$coef

  if (smry$df.residual) {
      pvals <- 2*pt(-abs(coef[,3]), smry$df.residual)
  } else {
      pvals <- 2*pnorm(-abs(coef[,3]))
  }

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,pvals,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  null.model <- update(obj, paste(names(obj$model[1]), " ~ 1"))

  LR <- deviance(null.model) - deviance(obj)
  df <- null.model$df.residual - smry$df.resid

  ll <- logLik(obj)
  dev <- deviance(obj)

  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(-deviance(null.model)/N)
    #LM.pwr <- exp(-smry$deviance/N)

    McFadden <- 1 - dev/deviance(null.model)
    Cox.Snell <- 1 - exp(-LR/N)
    Nagelkerke <- Cox.Snell/(1-L0.pwr)
    }
  else {
    LR <- NA
    df <- NA
    p <- NA
    McFadden <- NA
    Cox.Snell <- NA
    Nagelkerke <- NA
    }

  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          LR             = LR,
          df         = df,
          p             = p,
          logLik        = ll,
          deviance      = dev,
          McFadden      = McFadden,
          Cox.Snell       = Cox.Snell,
          Nagelkerke    = Nagelkerke,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=smry$xlevels,call=obj$call)
}

getSummary.simex <- function(obj,
            alpha=.05,
            ...){

  smry <- summary(obj)
  modsmry <- summary(obj$model)
  N <- if(length(weights(obj$model))) sum(weights(obj$model))
    else {
        if(length(modsmry$df) > 1) sum(modsmry$df[1:2])
        else obj$model$nobs
    }

  ## Asymptotics not guaranteed to be here.
  coef <- smry$coef$jackknife

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  sumstat <- c(N=N,
               LR=NA,
               df=NA,
               p=NA,
               McFadden=NA,
               Cox.Snell=NA,
               Nagelkerke=NA,
               logLik=NA,
               deviance=NA,
               AIC=NA,
               BIC=NA)

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=smry$xlevels,call=obj$call)
}

## The following two functions are contributed by
## Dave Atkins, PhD
## Research Associate Professor
## Center for the Study of Health and Risk Behaviors
## Department of  Psychiatry and Behavioral Science
## Unversity of Washington

getSummary.expcoef.glm <- function (obj, alpha = 0.05, ...)
{
    smry <- summary(obj)
    N <- if (length(weights(obj)))
        sum(weights(obj))
    else sum(smry$df[1:2])
    coef <- cbind(exp(smry$coef[,1]), smry$coef[,2:4]) # NOTE: exponentiated coefficients
    lower <- exp(qnorm(p = alpha/2, mean = smry$coef[, 1], sd = coef[,
        2]))
    upper <- exp(qnorm(p = 1 - alpha/2, mean = smry$coef[, 1], sd = coef[,
        2]))
    # NOTE: exponentiated CI
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    phi <- smry$dispersion
    LR <- smry$null.deviance - smry$deviance
    df <- smry$df.null - smry$df.residual
    ll <- logLik(obj)
    deviance <- deviance(obj)
    if (df > 0) {
        p <- pchisq(LR, df, lower.tail = FALSE)
        L0.pwr <- exp(-smry$null.deviance/N)
        McFadden <- 1 - smry$deviance/smry$null.deviance
        Cox.Snell <- 1 - exp(-LR/N)
        Nagelkerke <- Cox.Snell/(1 - L0.pwr)
    }
    else {
        LR <- NA
        df <- NA
        p <- NA
        McFadden <- NA
        Cox.Snell <- NA
        Nagelkerke <- NA
    }
    AIC <- AIC(obj)
    BIC <- AIC(obj, k = log(N))
    sumstat <- c(phi = phi, LR = LR, df = df, p = p, logLik = ll,
        deviance = deviance, McFadden = McFadden, Cox.Snell = Cox.Snell,
        Nagelkerke = Nagelkerke, AIC = AIC, BIC = BIC, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts,
        xlevels = obj$xlevels, call = obj$call)
}

getSummary.lmer <- function (obj, alpha = 0.05, ...)
{
    smry <- summary(obj)
    #N <- if (length(weights(obj))) ### NOTE: how to deal with groups/samp size?
    #    sum(weights(obj))
    #else sum(smry$df[1:2])
    coef <- smry@coefs
    lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,
        2])
    upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,
        2])
    if (ncol(smry@coefs) == 3) {
      p <- (1 - pnorm(smry@coefs[,3]))*2 # NOTE: no p-values for lmer() due to
                          # unclear dfs; calculate p-values based on z
      coef <- cbind(coef, p, lower, upper)
    } else {
        coef <- cbind(coef, lower, upper) # glmer will have 4 columns with p-values
        }
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    #phi <- smry$dispersion
    #LR <- smry$null.deviance - smry$deviance
    #df <- smry$df.null - smry$df.residual
    ll <- logLik(obj)[1]
    deviance <- deviance(obj)
    #if (df > 0) {
    #    p <- pchisq(LR, df, lower.tail = FALSE)
    #    L0.pwr <- exp(-smry$null.deviance/N)
    #    McFadden <- 1 - smry$deviance/smry$null.deviance
    #    Cox.Snell <- 1 - exp(-LR/N)
    #    Nagelkerke <- Cox.Snell/(1 - L0.pwr)
    #}
    #else {
    #    LR <- NA
    #    df <- NA
    #    p <- NA
    #    McFadden <- NA
    #    Cox.Snell <- NA
    #    Nagelkerke <- NA
    #}
    AIC <- smry@AICtab[1][,1] # NOTE: these are both data.frames? not sure why...
    BIC <- smry@AICtab[2][,1]
    ### NOTE: don't see a similar slot for "xlevels" to get levels of
    ###   factor variables used as predictors; for time being, force
    ###   user to specify explicitly
    #if (fac != NULL) {
    #   n <- length(fac)
    # xlevels <- vector(n, mode = "list")
    # for (i in 1:n) {
    #   xlevels[i] <- levels(obj@frame[,fac[i]])
    #   }
    # }
    #sumstat <- c(phi = phi, LR = LR, df = df, p = p, logLik = ll,
    #    deviance = deviance, McFadden = McFadden, Cox.Snell = Cox.Snell,
    #    Nagelkerke = Nagelkerke, AIC = AIC, BIC = BIC, N = N)
    sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC, BIC = BIC)
    list(coef = coef, sumstat = sumstat,
       contrasts = attr(model.matrix(obj), "contrasts"),
        xlevels = NULL, call = obj@call)
}

