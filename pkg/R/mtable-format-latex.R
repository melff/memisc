mtable_format_latex <- function(x,
          useDcolumn=getOption("useDcolumn",TRUE),
          colspec=if(useDcolumn)
                      paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
                  else "l",
          LaTeXdec=".",
          ddigits=min(3,getOption("digits")),
          useBooktabs=getOption("useBooktabs",TRUE),
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = " $\\times$ ",
          sdigits=min(1,ddigits),
          compact=FALSE,
          sumry.multicol=FALSE,
          ...)
    pf_mtable_format_latex(preformat_mtable(x),
          useDcolumn=useDcolumn,
          colspec=colspec,
          LaTeXdec=LaTeXdec,
          ddigits=ddigits,
          useBooktabs=useBooktabs,
          toprule=toprule,
          midrule=midrule,
          cmidrule=cmidrule,
          bottomrule=bottomrule,
          interaction.sep = interaction.sep,
          sdigits=sdigits,
          compact=compact,
          sumry.multicol=sumry.multicol,
          ...)


pf_mtable_format_latex <- function(x,
          useDcolumn=getOption("useDcolumn",TRUE),
          colspec=if(useDcolumn)
                      paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
                  else "l",
          LaTeXdec=".",
          ddigits=min(3,getOption("digits")),
          useBooktabs=getOption("useBooktabs",TRUE),
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = " $\\times$ ",
          sdigits=min(1,ddigits),
          compact=FALSE,
          sumry.multicol=FALSE,
          ...
          ){

    colsep <- " & "
    rowsep <- "\n"
    tabcr <- "\\\\"

    pt <- x$parmtab
    sst <- x$summary.stats
    sh <- x$sect.headers
    leaders <- x$leaders
    headers <- x$headers

    ncols <- sapply(pt[1,],ncol)
    if(missing(compact) && all(ncols)==1) compact = TRUE

    if(compact) modelsep <- " & "
    else modelsep <- " && "
    
    sh.nonnull <- !Sapply(sh,is.null)
    need.sh <- apply(sh.nonnull,1,any)
    if(length(sst))
        need.sh <- c(need.sh,FALSE)
    res <- NULL
    sec.hrule <- array(list(),dim=dim(sh))
    
    for(j in 1:ncol(pt)){
        
        pt.j <- pt[,j]
        sh.j <- sh[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        span.j <- unique(sapply(sh.j,attr,"span"))
        if(!is.numeric(span.j)) span.j <- 1

        if(span.j > 1)
            pt.j <- lapply(pt.j,cols_folded,span.j,sep=colsep)

        for(i in 1:length(pt.j)){
            pt.ij <- pt.j[[i]]
            pt.ij <- centerAt(pt.ij,at=".",integers="dot")
            if(!useDcolumn)
                pt.ij[] <- paste0("$",pt.ij,"$")
            
            if(need.sh[[i]]){
                sh.ij <- sh.j[[i]]
                if(!length(sh.ij)) {
                    sh.ij <- ""
                    sec.hrule[[i,j]] <- list(draw=FALSE,length=ncol(pt.ij))
                }
                else
                    sec.hrule[[i,j]] <- list(draw=TRUE,length=ncol(pt.ij))

                sh.ij <- set_length(sh.ij,ncol.j/span.j)
                sh.ij <- if(span.j==1 && !nzchar(sh.ij)) ""
                         else paste0("\n\\multicolumn{",span.j,"}{c}{",sh.ij,"}")
                pt.ij <- rbind(sh.ij,pt.ij)
            }
            pt.ij <- apply(pt.ij,1,paste,collapse=colsep)
            pt.j[[i]] <- pt.ij
        }

        pt.j <- unlist(pt.j)
        if(length(sst)){
            sst.j <- sst[[j]]
            if(!useDcolumn){
                sst.j <- centerAt(sst.j,at=".",integers="dot")
                sst.j <- paste0("$",sst.j,"$")
            }
            else if(sumry.multicol){
                has.dot <- grep(".",sst.j,fixed=TRUE)
                sumry.spec <- if(useDcolumn && has.dot)
                                  paste("D{.}{",LaTeXdec,"}{",sdigits,"}",sep="")
                              else "r"
                sst.j <- paste0("\\multicolumn{1}{",sumry.spec,"}{",sst.j,"}")
            }
            sst.j <- as.matrix(sst.j)
            sst.j <- centerAt(sst.j,at=".",integers="dot")
            sst.j <- colexpand(sst.j,ncol.j)
            sst.j <- apply(sst.j,1,paste,collapse=colsep)
            pt.j <- c(pt.j,sst.j)
        }

        res <- cbind(res,pt.j)
    }

    l.leaders <- length(leaders)
    if(l.leaders){

        for(i in 1:length(leaders)){
            if(need.sh[i]){
                leaders.i <- leaders[[i]]
                leaders[[i]] <- c(list(structure("",span=1)),leaders.i)
            }
        }
        
        leaders <- lapply(leaders,ldxp)
        leaders <- do.call(rbind,leaders)
        leaders <- gsub(" x ",interaction.sep,leaders,fixed=TRUE)
        leaders <- format(leaders,justify="left")
        leaders <- gsub("$\\"," $\\",leaders,fixed=TRUE)
        res <- cbind(leaders,res)
    }
    
    res <- apply(res,1,paste,collapse=modelsep)

    wdld <- as.integer(nzchar(leaders[1,]))
    l.headers <- length(headers)
    hi.rules <- character()
    if(length(headers)){

        headers1 <- Map(structure,list(""),span=ncols) # To take care of multi-eqn models
        headers <- c(headers,list(headers1))
        headers <- rev(headers)
        hspan <- list()
        
        for(k in 1:length(headers)){
            hspan[[k]] <- sapply(headers[[k]],attr,"span")
        }
        ghspan <- mkGrpSpan(hspan)
        headers <- headers[-1]
        ghspan <- ghspan[-1]

        headers <- rev(headers)
        hspan <- rev(hspan)
        ghspan <- rev(ghspan)

        if(compact) multip <- 1
        else multip <- 2
        for(k in 1:l.headers){

            header.k <- unlist(headers[[k]])
            hspan.k <- hspan[[k]]
            ghspan.k <- (ghspan[[k]]-1)*multip
            hspan.k <- hspan.k + ghspan.k 
            header.k <- paste0("\n\\multicolumn{",hspan.k,"}{c}{",header.k,"}")
            if(l.leaders)
                header.k <- c("",header.k)
            headers[[k]] <- paste(header.k,collapse=modelsep)
            
            hi.rule.k.start <- wdld  + cumsum(c(1,hspan.k[-1]) + as.integer(!compact))
            hi.rule.k.end <- hi.rule.k.start + hspan.k - 1
            hi.rules.k <- paste0(cmidrule,"{",hi.rule.k.start,"-",hi.rule.k.end,"}")
            hi.rules.k <- paste(hi.rules.k,collapse="")
            hi.rules[k] <- hi.rules.k
        }
        headers <- unlist(headers)
        res <- c(headers,res)
    }
    
    res <- paste0(res,tabcr)


    l.headers <- length(headers)

    csum <- l.headers

    sectsep.at <- integer()
    sectseps <- character()

    sec.hrules.at <- integer()
    sec.hrules <- character()

    sectionrule <- midrule
    
    if(compact) leaders.gap <- 1
        else leaders.gap <- 2
    for(i in 1:nrow(pt)){
        if(need.sh[i]){
            sec.hrules.i <- sec.hrule[i,]
            sec.hrules.i.draw <- sapply(sec.hrules.i,"[[","draw")
            sec.hrules.i.length <- sapply(sec.hrules.i,"[[","length")
            sec.hrules.i.stepsize <- if(compact)
                                         sec.hrules.i.length
                                     else
                                         sec.hrules.i.length + 1
            l <- length(sec.hrules.i.stepsize)
            sec.hrules.i.start <- leaders.gap + cumsum(c(1,sec.hrules.i.stepsize[-l]))
            sec.hrules.i.end   <- sec.hrules.i.start + sec.hrules.i.length - 1
            sec.hrules.i <- paste0(cmidrule,"{",sec.hrules.i.start,"-",sec.hrules.i.end,"}")
            sec.hrules.i[!sec.hrules.i.draw] <- ""
            sec.hrules.i <- paste(sec.hrules.i,collapse="")
            sec.hrules    <- c(sec.hrules,    sec.hrules.i)
            sec.hrules.at <- c(sec.hrules.at, csum)
            csum <- csum + 1
        }
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
        csum <- csum + nrow(pt[[i,1]])
    }
    if(length(sst)){
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
    }

    res <- .insert(res,
                   c(sec.hrules.at,sectsep.at),
                   c(sec.hrules,   sectseps)
                   )
    
    if(l.headers > 1){
        hi.rules.at <- seq.int(l.headers-1)
        res <- .insert(res,hi.rules.at,hi.rules)
    }
    res <- c(toprule,res,bottomrule)

    tab.spec <- character()
    
    if(l.leaders)
        tab.spec <- c(tab.spec,"l")
    for(j in 1:ncol(pt)){
        if(!compact) tab.spec <- c(tab.spec,"c")
        ncol.j <- ncol(pt[[1,j]])
        tab.spec <- c(tab.spec,paste(rep(colspec,ncol.j),collapse=""))
    }
    tab.spec <- paste(tab.spec,collapse="")
    
    tabbegin <- paste("\\begin{tabular}{",tab.spec,"}",sep="")
    tabend <- "\\end{tabular}"

    res <- c(tabbegin,res,tabend)
    
    return(res)
    
}



