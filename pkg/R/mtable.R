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

names.or.rownames <- function(x){
    if(is.array(x)) rownames(x)
    else names(x)
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
                    sdigits=digits,
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
  
  if(length(gs.options)){
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
  
  parameter.types <- unique(unlist(lapply(summaries,names)))
  parameter.types <- parameter.types[parameter.types %nin% c("sumstat","contrats","call")]
  parmnames <- list()
  for(pt in parameter.types){

      tmp.pn <- lapply(summaries,`[[`,pt)
      tmp.pn <- lapply(tmp.pn,names.or.rownames)
      parmnames[[pt]] <- unique(unlist(tmp.pn))
  }
  parameter.names <- unique(unlist(parmnames))
  
  stemplates <- lapply(args,getSummaryTemplate)
  
  structure(summaries,
            names=argnames,
            class="memisc_mtable",
            parameter.names=parameter.names,
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
    adims <- if(length(dim(parm))==2) 1 else c(1,3)
    if(length(parm)){
        
        if(is.array(parm)){
            ans <- apply(parm,adims,applyTemplate,
                         template=template,
                         float.style=float.style,
                         digits=digits,
                         signif.symbols=signif.symbols)
        }
        else {
            ans <- array(formatC(parm,
                                 digits=digits,
                                 ifelse(is.integer(parm),
                                        "d","f"),
                                 width=1),
                         dim=c(1,1,length(parm),1),
                         dimnames=list("1","2",names(parm),"3"))
            return(ans)
        }
    }
    else {
        ans <- array(character(0),
                     dim=c(0,dim(parm)[adims]),
                     dimnames=c(list(NULL),dimnames(parm)[adims]))
    }
        
    if(length(dim(template))){
        newdims <- c(dim(template),dim(ans)[-1])
        newdimnames <- c(dimnames(template),dimnames(ans)[-1])

        for(i in 1:length(newdims)){
            if(!length(newdimnames[[i]])){
                if(newdims[i]==0)
                    newdimnames[[i]] <- character(0)
                else
                    newdimnames[[i]] <- as.character(1:newdims[i])
            }
        }
        
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
    x[r,,drop=FALSE]
}
get_rows <- function(x,i)try(x[i,,drop=FALSE])

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

do_subs <- function(x,r){
    for(rr in r)
        x <- do_1sub(x,rr)
    return(x)
}

do_1sub <- function(x,r){

    r.gsub <- r$gsub
    r.fixed <- r$fixed

    r <- r[names(r)%nin%c("gsub","fixed","warn")]

    y <- x
    for(i in seq_along(r)){
        from <- names(r)[i]
        to <- r[[i]]
        if(r.gsub)
            y <- gsub(from,to,y,fixed=r.fixed)
        else {
            y[y==from] <- to
        }
    }
    return(y)
}

do_prettyfy <- function(pn,
                        contrasts,     
                        xlevels,         
                        factor.style,    
                        show.baselevel,
                        baselevel.sep){

    if(!length(contrasts)) return(pn)
    
    res <- pn

    done <- res != pn

    for(m in names(contrasts)){
        contrasts.m <- contrasts[[m]]
        xlevels.m <- xlevels[[m]]

        pn.tmp <- pn[!done]
        pn.tmp  <- prettyNames(pn.tmp,
                               contrasts=contrasts.m,
                               xlevels=xlevels.m,
                               factor.style=factor.style,
                               show.baselevel=show.baselevel,
                               baselevel.sep=baselevel.sep)
        res[!done] <- pn.tmp
        done <- res != pn
    }
    
    return(res)
}    

nzchar_row <- function(x){
    nzch <- array(nzchar(x),dim=dim(x))
    apply(nzch,1,any)
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
    
    modelnames <- names(x)
    modelgroups <- attr(x,"model.groups")

    parmtab <- NULL
    sect.headers <- NULL
    if(length(partypes)){

        for(n in 1:length(parms)){

            parms.n <- parms[[n]]
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
            modnames <- names(mod)
            for(m in modnames){
                mod.m <- mod[[m]]
                if(length(mod.m))
                    parmtab[[m,n]] <- mod.m
            }
            
        }

        parameter.names <- attr(x,"parameter.names")
        parmnames <- list()
        
        for(m in rownames(parmtab)){
            tmp.pn <- lapply(parmtab[m,],dimnames3)
            tmp.pn <- unique(unlist(tmp.pn))
            tmp.pn <- parameter.names[parameter.names %in% tmp.pn]
            parmnames[[m]] <- tmp.pn
        }
        
        
        for(n in 1:ncol(parmtab)){
            mod <- parms[[n]]
            for(m in rownames(parmtab)){
                parmtab.mn <- parmtab[[m,n]]
                if(length(parmnames[[m]])){
                    parmtab.mn <- coefxpand(parmtab.mn,parmnames[[m]])
                    parmtab.mn <- prefmt2(parmtab.mn)
                }
                parmtab[[m,n]] <- parmtab.mn
                modm <- mod[[m]]
                if(length(dim(modm))>3){
                    nc.mn <- max(ncol(parmtab.mn),1)
                    d4 <- dim(modm)[4]
                    dn4 <- dimnames(modm)[[4]]
                    dn4 <- do_subs(dn4,relab.attr)
                    sect.headers[[m,n]] <- structure(dn4,span=nc.mn/d4)
                }
            }
            maxncol <- max(unlist(lapply(parmtab[,n],ncol)) )
            parmtab[,n] <- lapply(parmtab[,n],colexpand,maxncol)

            sh <- sect.headers[,n]
            maxl <- max(unlist(lapply(sh,length)))
            sh <- lapply(sh,`length<-`,maxl)
        }
        
        for(m in 1:nrow(parmtab)){
            parmtab.m <- parmtab[m,,drop=FALSE]
            maxnrow <- max(unlist(lapply(parmtab.m,nrow)) )
            parmtab.n <- lapply(parmtab.m,rowexpand,maxnrow)
            nzch <- Sapply(parmtab.m,nzchar_row)
            nzch <- apply(nzch,1,any)
            parmtab.m <- lapply(parmtab.m,get_rows,i=nzch)
            parmtab[m,] <- parmtab.m
        }
    
        for(m in rownames(sect.headers)){
            sh <- sect.headers[m,]
            if(!getOption("mtable.always.eqnames",FALSE) && length(unique(unlist(sh)))==1){
                sh <- list(NULL)
            }
            sect.headers[m,] <- sh
        }
    }

    headers <- list()
    force.header <- getOption("mtable.force.header",default=FALSE)
    if(length(modelnames) > 1 || length(modelnames) == 1 && force.header) {
        modelnames <- do_subs(modelnames,relab.attr)
        headers[[1]] <- Map(structure,modelnames,span=lapply(parmtab[1,],ncol))
        if(length(modelgroups)){
            ncols <- sapply(parmtab[1,],ncol)
            sp <- lapply(modelgroups,function(mg)sum(ncols[mg]))
            h <- Map(structure,names(modelgroups),span=sp)
            headers <- c(list(h),headers)
        }
    }

    leaders <- character(0)
    if(length(partypes))
        leaders <- structure(lapply(rownames(parmtab),
                                function(m){
                                    pn <- parmnames[[m]]
                                    pn <- do_prettyfy(pn,
                                                      contrasts=contrasts,     
                                                      xlevels=xlevels,         
                                                      factor.style=factor.style,    
                                                      show.baselevel=show.baselevel,
                                                      baselevel.sep=baselevel.sep)  
                                    pn <- do_subs(pn,relab.attr)
                                    span <- nrow(parmtab[[m,1]])/length(pn)
                                    if(span < 1) browser()
                                    lapply(pn,structure,span=span)
                                }),
                             names=rownames(parmtab))

    if(isTRUE(summary.stats) || is.character(summary.stats) && length(summary.stats)) {
        sumstats <- Map(applyTemplate,sumstats,stemplates,digits=sdigits)
        if(is.character(summary.stats))
            sst <- lapply(sumstats,getRows,summary.stats)
        else
            sst <- sumstats

        snames <- unique(unlist(lapply(sst,rownames)))
        nc <- lapply(parmtab[1,],ncol)
        summary.stats <- Map(smryxpand,sst,list(snames))

        snames <- do_subs(snames,relab.attr)
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


