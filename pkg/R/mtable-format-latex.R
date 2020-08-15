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
          escape.tex=getOption("toLatex.escape.tex",FALSE),
          signif.notes.type=getOption("toLatex.signif.notes.type","include"),
          signif.notes.spec=getOption("toLatex.signif.notes.spec","p{.5\\linewidth}"),
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
          escape.tex=escape.tex,
          signif.notes.type=signif.notes.type,
          signif.notes.spec=signif.notes.spec,
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
          escape.tex=getOption("toLatex.escape.tex",FALSE),
          signif.notes.type=getOption("toLatex.signif.notes.type","include"),
          signif.notes.spec=getOption("toLatex.signif.notes.spec","p{.5\\linewidth}"),
          ...
          ){

    signif.notes.type <- match.arg(signif.notes.type,
                                           c("include","drop","append","tnotes","drop"))
    
    colsep <- " & "
    rowsep <- "\n"
    tabcr <- "\\\\"

    pt <- x$parmtab
    sst <- x$summary.stats
    sh <- x$sect.headers
    leaders <- x$leaders
    headers <- x$headers
    outtypes <- x$outtypes
    
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
            ot.ij <- outtypes[i,j]
            pt.ij <- pt.j[[i]]
            if(ot.ij == "num"){
                pt.ij <- sub("(\\*+)","^{\\1}",pt.ij)
                pt.ij <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",pt.ij)
                pt.ij <- centerAt(pt.ij,at=".",integers="dot")
                if(!useDcolumn)
                    pt.ij[] <- paste0("$",pt.ij,"$")
            } else {
                if(useDcolumn)
                    pt.ij[] <- paste0("\\multicolumn{1}{c}{",pt.ij,"}")
            }
            
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
                if(escape.tex)
                    sh.ij <- LaTeXcape(sh.ij)
                else
                    checkLaTeXcape(sh.ij,"group headings")
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
        if(escape.tex)
            leaders <- LaTeXcape(leaders)
        else
            checkLaTeXcape(leaders,"row leaders")
        leaders <- format(leaders,justify="left")
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
            if(escape.tex)
                header.k <- LaTeXcape(header.k)
            else
                checkLaTeXcape(header.k,"column headings")
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
    if(length(sst) && any(sapply(sst,length)>0)){
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
        tab.spec <- c(tab.spec,rep(colspec,ncol.j))
    }

    total.ncol <- length(tab.spec)
    tab.spec <- paste(tab.spec,collapse="")

    signif.symbols <- x$signif.symbols
    if(length(signif.symbols)){
        signif.template <- getOption("signif.symbol.toLatex.template",
                                     signif.symbol.toLatex.default.template)
        signif.symbols <- format_signif_print(signif.symbols,
                                              signif.template,
                                              width=nchar(bottomrule))
        if(signif.notes.type=="include"){
            signif.symbols <- paste0("\\multicolumn{",total.ncol,"}{",
                                     signif.notes.spec,"}{",
                                     paste0(signif.symbols,collapse="\n"),
                                 "}\\\\")
            res <- c(res, signif.symbols)
        }
    }
    
    tabbegin <- paste("\\begin{tabular}{",tab.spec,"}",sep="")
    tabend <- "\\end{tabular}"

    res <- c(tabbegin,res,tabend)
    if(length(signif.symbols)){
        if(signif.notes.type=="append"){
            signif.symbols <- paste0(signif.symbols,collapse="\n")
            res <- c(res,"\\par",signif.symbols,"\\par")
        }
        else if(signif.notes.type=="tnotes"){
            signif.symbols <- c(
                "\\begin{tablenotes}",
                paste0(c("\\item",signif.symbols),collapse="\n"),
                "\\end{tablenotes}"
            )
            res <- c(
                "\\begin{threeparttable}",
                res,
                signif.symbols,
                "\\end{threeparttable}"
                )
        }
    }
    
    return(res)
    
}


signif.symbol.toLatex.default.template <- c("Significance: ","$$sym \\equiv p < $val$","; ")

