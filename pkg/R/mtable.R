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
  CoefTemplates <- get("CoefTemplates", envir=.memiscEnv)
  OldCoefTemplates <- CoefTemplates
    for(coef.style in argnames){
      CoefTemplates[[coef.style]] <- args[[coef.style]]
  }
  assign("CoefTemplates",CoefTemplates, envir=.memiscEnv)
  return(invisible(OldCoefTemplates))
}

getFirstMatch <- function(x,n){
  for(n. in n){
    if(n. %in% names(x)) return(x[[n.]])
  }
  return(x[["default"]])
}

getCoefTemplate <- function(style){
  CoefTemplates <- get("CoefTemplates", envir=.memiscEnv)
  if(missing(style)) return(CoefTemplates)
  else return(CoefTemplates[[style]])
}


getSummary <- function(obj,alpha=.05,...) UseMethod("getSummary")
# setGeneric("getSummary")


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
  N <- if(length(weights(obj))) sum(weights(obj),na.rm=TRUE)
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

    Aldrich.Nelson <- LR/(LR+N)
    McFadden <- 1- smry$deviance/smry$null.deviance
    Cox.Snell <- 1 - exp(-LR/N)
    Nagelkerke <- Cox.Snell/(1-L0.pwr)
    }
  else {
    LR <- NA
    df <- NA
    p <- NA
    Aldrich.Nelson <- NA
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
          Aldrich.Nelson = Aldrich.Nelson,
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

summaryTemplate <- function(x)
  UseMethod("summaryTemplate")

getSummaryTemplate <- function(x){
  SummaryTemplates <- get("SummaryTemplates", envir=.memiscEnv)
  if(missing(x)) return(SummaryTemplates)
  if(is.character(x)) cls <- x
  else cls <- class(x)
  stf <- getS3method("summaryTemplate",cls,optional=TRUE)
  if(length(stf))
    res <- stf(x)
  else
    res <- getFirstMatch(SummaryTemplates,cls)
  return(res)
}

setSummaryTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  OldSummaryTemplates <- SummaryTemplates <- get("SummaryTemplates", envir=.memiscEnv)
  for(cls in argnames){
      SummaryTemplates[[cls]] <- args[[cls]]
  }
  assign("SummaryTemplates",SummaryTemplates,envir=.memiscEnv)
  return(invisible(OldSummaryTemplates))
}

prettyNames <- function(coefnames,
                        contrasts,
                        xlevels,
                        factor.style,
                        show.baselevel,
                        baselevel.sep
                        ){
    termorders <- sapply(strsplit(coefnames,":",fixed=TRUE),length)
    ordergroups <- split(coefnames,termorders)
    ordergroups <- lapply(ordergroups,prettyNames1,
                        contrasts=contrasts,
                        xlevels=xlevels,
                        factor.style=factor.style,
                        show.baselevel=show.baselevel,
                        baselevel.sep=baselevel.sep
                        )
    unsplit(ordergroups,termorders)
}

prettyNames1 <- function(str,
                        contrasts,
                        xlevels,
                        factor.style,
                        show.baselevel,
                        baselevel.sep
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
         if(show.baselevel)
           newlabels <- paste(colnames(contrast.matrix),baselevel,sep=baselevel.sep)
         else
           newlabels <- colnames(contrast.matrix)
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
         if(show.baselevel)
           newlabels <- paste(colnames(contrast.matrix),baselevel,sep=baselevel.sep)
         else
           newlabels <- colnames(contrast.matrix)
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
                    signif.symbols=getOption("signif.symbols"),
                    factor.style=getOption("factor.style"),
                    show.baselevel=getOption("show.baselevel"),
                    baselevel.sep=getOption("baselevel.sep"),
                    getSummary=eval.parent(quote(getSummary)),
                    float.style=getOption("float.style"),
                    digits=min(3,getOption("digits")),
                    sdigits=min(1,digits),
                    gs.options=NULL
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
  
  if(length(options)){
    summaries.call <- as.call(
      c(list(as.name("lapply"),
             as.name("args"),
             FUN=as.name("getSummary")),
        gs.options
      ))
    summaries <- eval(summaries.call)
  }
  else
    summaries <- lapply(args,getSummary)

  stemplates <- lapply(args,getSummaryTemplate)
  
  structure(summaries,
            names=argnames,
            class="memisc_mtable",
            coef.style=coef.style,
            summary.stats=summary.stats,
            signif.symbols=signif.symbols,
            factor.style=factor.style,
            show.baselevel=show.baselevel,
            baselevel.sep=baselevel.sep,
            float.style=float.style,
            digits=digits,
            stemplates=stemplates,
            sdigits=sdigits
            )
  
}

prefmt1 <- function(parm,template,float.style,digits,signif.symbols){
    if(!length(parm)) return(NULL)
    adims <- if(length(dim(parm))==2) 1 else c(1,3)
    ans <- apply(parm,adims,applyTemplate,
                 template=template,
                 float.style=float.style,
                 digits=digits,
                 signif.symbols=signif.symbols)
    if(length(dim(template))){
        newdims <- c(dim(template),dim(ans)[-1])
        newdimnames <- c(dimnames(template),dimnames(ans)[-1])
        newdimnames <- lapply(1:length(newdims),function(i){
            if(length(newdimnames[[i]])) return(newdimnames[[i]])
            else return(as.character(1:newdims[i]))
        })
        dim(ans) <- newdims
        dimnames(ans) <- newdimnames
    } else rownames(ans) <- names(template)
    ans[ans=="()"] <- ""
    return(ans)
}

prefmt2 <- function(parm){
    
    if(length(dim(parm))<4)
        dim(parm)[4] <- 1

    parm <- aperm(parm,c(1,3,2,4))
    dim(parm) <- c(prod(dim(parm)[1:2]),prod(dim(parm)[3:4]))

    parm
}

colexpand <- function(x,nc){
    x.nr <- nrow(x)
    x.nc <- ncol(x)
    y <- matrix("",nrow=x.nr,ncol=max(nc,1))
    if(length(x))
        y[,1:x.nc] <- x
    y
}

rowexpand <- function(x,nr){
    x.nr <- nrow(x)
    x.nc <- ncol(x)
    y <- matrix("",nrow=nr,ncol=x.nc)
    if(length(x))
        y[1:x.nr,] <- x
    y
}


dimnames3 <- function(x)dimnames(x)[[3]]

getRows <- function(x,r){
    r <- intersect(r,rownames(x))
    y <- try(x[r,,drop=FALSE])
    if(inherits(y,"try-error")) browser()
    y
}

relabel.memisc_mtable <- function(x,...,gsub=FALSE,fixed=!gsub,warn=FALSE){

    relab.req <- list(...,
                      gsub=gsub,fixed=fixed,warn=warn)
    
    relab.attr <- attr(x,"relabel")
    if(!length(relab.attr))
        relab.attr <-list(relab.req)
    else
        relab.attr <-c(relab.attr,
                       list(relab.req))

    attr(x,"relabel") <- relab.attr

    x
}

pt_getrow <- function(x,i){
    y <- x[i,]
    isn <- sapply(y,is.null)
    if(any(isn)) return(y[!isn])
    else return(y)
}

preformat_mtable <- function(x){

    x <- unclass(x)
    
    coef.style <- attr(x,"coef.style")
    summary.stats <- attr(x,"summary.stats")
    signif.symbols <- attr(x,"signif.symbols")
    factor.style <- attr(x,"factor.style")
    show.baselevel <- attr(x,"show.baselevel")
    baselevel.sep <- attr(x,"baselevel.sep")
    float.style <- attr(x,"float.style")
    digits <- attr(x,"digits")
    stemplates <- attr(x,"stemplates")
    sdigits <- attr(x,"sdigits")

    allcompo <- unique(unlist(lapply(x,names)))
    nonparnames <- c("sumstat","contrasts","xlevels","call")
    partypes <- setdiff(allcompo,nonparnames)

    sumstats <- lapply(x,`[[`,"sumstat")
    contrasts <- lapply(x,`[[`,"contrasts")
    xlevels <- lapply(x,`[[`,"xlevels")
    calls <- lapply(x,`[[`,"call")
    parms <- lapply(x,`[`,partypes)

    ctemplate <- getCoefTemplate(coef.style)
    if(!length(ctemplate)) stop("invalid coef.style argument")
    ctemplate <- as.matrix(ctemplate)
    ctdims <- dim(ctemplate)
    lctdims <- length(ctdims)
    if(lctdims>2) stop("can\'t handle templates with dim>2")

    relab.attr <- attr(x,"relabel")
    
    for(r in relab.attr){
        x <- do.call("rename",c(list(x=x),r))
    }
    modelnames <- names(x)
    modelgroups <- attr(x,"model.groups")
    
    for(n in 1:length(parms)){

        parms.n <- parms[[n]]
        for(r in relab.attr){
            for(j in 1:length(parms.n)){
                pmj <- parms.n[[j]]
                rownames(pmj)  <- prettyNames(rownames(pmj),
                                              contrasts=contrasts[[n]],
                                              xlevels=xlevels[[n]],
                                              factor.style=factor.style,
                                              show.baselevel=show.baselevel,
                                              baselevel.sep=baselevel.sep)
                pmj  <- do.call("dimrename",c(list(x=pmj),r))
                parms.n[[j]] <- pmj
            }
        }
        
        parms[[n]] <- lapply(parms.n,
                             prefmt1,
                             template=ctemplate,
                             float.style=float.style,
                             digits=digits,
                             signif.symbols=signif.symbols)
    }
    
    sect.headers <- parmtab <-
        array(list(),
              dim=c(length(partypes),length(parms)),
              dimnames=list(partypes,names(parms)))

    for(n in 1:length(parms)){
        mod <- parms[[n]]
        m <- names(mod)
        for(m. in m){
            mod.m <- mod[[m]]
            if(length(mod.m))
                parmtab[[m,n]] <- mod.m
        }
        
    }
    parmnames <- structure(lapply(1:nrow(parmtab),
                                  function(i)
                                      unique(
                                          unlist(
                                              lapply(pt_getrow(parmtab,i),
                                                     dimnames3)))),
                           names=rownames(parmtab))

    parmnames.reordered <- parmnames # Make reordering possible - to be implemented later ...
    
    for(n in 1:ncol(parmtab)){
        mod <- parms[[n]]
        for(m in rownames(parmtab)){
            parmtab.mn <- parmtab[[m,n]]
            parmtab.mn <- coefxpand(parmtab.mn,parmnames.reordered[[m]])
            parmtab.mn <- prefmt2(parmtab.mn)
            parmtab[[m,n]] <- parmtab.mn

            modm <- mod[[m]]
            if(length(dim(modm))>3){
                nc.mn <- ncol(parmtab.mn)
                d4 <- dim(modm)[4]
                dn4 <- dimnames(modm)[[4]]
                sect.headers[[m,n]] <- structure(dn4,span=nc.mn/d4)
            }
        }
        maxncol <- max(unlist(lapply(parmtab[,n],ncol)) )
        parmtab[,n] <- lapply(parmtab[,n],colexpand,maxncol)
    }
    
    for(m in 1:nrow(parmtab)){
        maxnrow <- max(unlist(lapply(parmtab[m,],nrow)) )
        parmtab[m,] <- lapply(parmtab[m,],rowexpand,maxnrow)
    }
    
    
    for(m in rownames(sect.headers)){
        sh <- sect.headers[m,]
        maxl <- max(unlist(lapply(sh,length)))
        sh <- lapply(sh,`length<-`,maxl)
        sect.headers[m,] <- sh
    }

    headers <- list()
    force.header <- getOption("mtable.force.header",default=FALSE)
    if(length(modelnames) > 1 || length(modelnames) == 1 && force.header) {
        headers[[1]] <- Map(structure,modelnames,span=lapply(parmtab,ncol))
        if(length(modelgroups)){
            ncols <- sapply(parmtab[1,],ncol)
            sp <- lapply(modelgroups,function(mg)sum(ncols[mg]))
            h <- Map(structure,names(modelgroups),span=sp)
            headers <- c(list(h),headers)
        }
    }
    
    leaders <- structure(lapply(rownames(parmtab),
                                function(m){
                                        pn <- parmnames.reordered[[m]]
                                        span <- nrow(parmtab[[m,1]])/length(pn)
                                        lapply(pn,structure,span=span)
                                    }),
                             names=rownames(parmtab))
    maxl <- max(unlist(lapply(leaders,length)))
    leaders <- lapply(leaders,`length<-`,maxl)

    if(isTRUE(summary.stats) || is.character(summary.stats) && length(summary.stats)) {
        sumstats <- Map(applyTemplate,sumstats,stemplates,digits=sdigits)
        if(is.character(summary.stats))
            sst <- lapply(sumstats,getRows,summary.stats)
        else
            sst <- sumstats

        snames <- unique(unlist(lapply(sst,rownames)))
        nc <- lapply(parmtab[1,],ncol)
        summary.stats <- Map(smryxpand,sst,list(snames))

        snames <- lapply(snames,structure,span=1)
        leaders <- c(leaders,summary.stats=list(snames))
    }
    else summary.stats <- NULL

    structure(list(parmtab=parmtab,
                   leaders=leaders,
                   headers=headers,
                   sect.headers=sect.headers,
                   summary.stats = summary.stats),
              class="preformatted.memisc_mtable")
    }


format.memisc_mtable <- function(x,
                          target=c("print","LaTeX","HTML","delim"),
                          ...){
    target <- match.arg(target)
    x <- preformat_mtable(x)
    switch(target,
           print=pf_mtable_format_print(x,...),
           LaTeX=pf_mtable_format_latex(x,...),
           HTML=pf_mtable_format_html(x,...),
           delim=pf_mtable_format_delim(x,...)
           )
}

print.memisc_mtable <- function(x,center.at=getOption("OutDec"),
      topsep="=",bottomsep="=",sectionsep="-",...){

    calls <- sapply(x,"[[","call")
    cat("\nCalls:\n")
    for(i in seq(calls)){
        cat(names(calls)[i],": ",sep="")
        print(calls[[i]])
    }
    cat("\n")
    cat(format.memisc_mtable(x,target="print",
                      center.at=center.at,
                      topsep=topsep,
                      bottomsep=bottomsep,
                      sectionsep=sectionsep,...),
        sep="")
}

toLatex.memisc_mtable <- function(object,...){
  structure(format.memisc_mtable(x=object,target="LaTeX",...),
  class="Latex")
}

write.mtable <- function(object,file="",
                         format=c("delim","LaTeX","HTML"),
                         ...){
  l <- list(...)
  if(isTRUE(l[["forLaTeX"]])) # Avoid breaking old code
    target <- "LaTeX"
  else
    target <- match.arg(format)
    
  f <- format.memisc_mtable(object,target=target,...)
  if(target %in% c("LaTeX","HTML"))
    f <- paste(f,"\n",sep="")
  cat(f,file=file,sep="")
}


