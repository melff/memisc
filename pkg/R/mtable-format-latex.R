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
          signif.notes.spec=getOption("toLatex.signif.notes.spec",
                                      paste0("p{",signif.notes.width,"\\linewidth}")),
          signif.notes.width=getOption("toLatex.signif.notes.width",".7"),
          ...
          ){

    signif.notes.type <- match.arg(signif.notes.type,
                                           c("include","append","tnotes","drop"))
    
    colsep <- " & "
    rowsep <- "\n"
    tabcr <- "\\\\"

    pt <- x$parmtab
    sst <- x$summary.stats
    leaders <- x$leaders
    headers <- x$headers
    eq.headers <- x$eq.headers
    outtypes <- x$outtypes
    
    ncols <- sapply(pt[1,],ncol)
    if(missing(compact) && all(ncols==1))
        compact <- TRUE

    if(compact) modelsep <- " & "
    else modelsep <- " && "
    
    res <- NULL
    has.eq.headers <- length(eq.headers) > 0

    for(j in 1:ncol(pt)){
        
        name.j <- colnames(pt)[j]
        pt.j <- pt[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)

        for(i in 1:length(pt.j)){
            ot.ij <- outtypes[i,j]
            pt.ij <- pt.j[[i]]
            if(ot.ij == "num"){
                pt.ij <- sub("(\\*+)","^{\\1}",pt.ij)
                pt.ij <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",pt.ij)
                #pt.ij <- centerAt(pt.ij,at=".",integers="dot")
                if(!useDcolumn)
                    pt.ij[] <- paste0("$",pt.ij,"$")
            } else {
                if(useDcolumn)
                    pt.ij[] <- paste0("\\multicolumn{1}{c}{",pt.ij,"}")
            }
            
            pt.ij <- apply(pt.ij,1,paste,collapse=colsep)
            pt.j[[i]] <- pt.ij
        }
        pt.j <- unlist(pt.j)
        if(has.eq.headers){
            eq.header.j <- eq.headers[[name.j]]
            n.eq.j <- length(eq.header.j)
            eq.span <- ncol.j/n.eq.j
            eq.header.j <- paste0("\\multicolumn{",eq.span,"}{c}{",eq.header.j,"}")
            eq.header.j <- paste(eq.header.j,collapse=" & ")
            pt.j <- c(eq.header.j,pt.j)
        }
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
        leaders <- lapply(leaders,ldxp)
        leaders <- do.call(rbind,leaders)
        leaders <- gsub(" x ",interaction.sep,leaders,fixed=TRUE)
        if(escape.tex)
            leaders <- LaTeXcape(leaders)
        else
            checkLaTeXcape(leaders,"row leaders")
        if(has.eq.headers)
            leaders <- rbind("",leaders)
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

        if(!compact)
            wdld <- wdld + 1
        for(k in 1:l.headers){

            header.k <- unlist(headers[[k]])
            if(escape.tex)
                header.k <- LaTeXcape(header.k)
            else
                checkLaTeXcape(header.k,"column headings")
            hspan.k <- hspan[[k]]
            ghspan.k <- ghspan[[k]]
            header.k <- paste0("\n\\multicolumn{",hspan.k,"}{c}{",header.k,"}")
            if(l.leaders)
                header.k <- c("",header.k)
            headers[[k]] <- paste(header.k,collapse=modelsep)

            n.grps <- length(hspan.k)
            if(!compact){
                hspan.k <- hspan.k + ghspan.k - 1
                hi.rule.k.start <- cumsum(c(1,hspan.k[-n.grps] + 1)) + wdld
            }
            else
                hi.rule.k.start <- cumsum(c(1,hspan.k[-n.grps])) + wdld
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
    
    for(i in 1:nrow(pt)){
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
        csum <- csum + nrow(pt[[i,1]])
    }
    if(length(sst) && any(sapply(sst,length)>0)){
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
    }
    if(has.eq.headers){
        sectsep.at <- sectsep.at + 1
    }
    
    res <- .insert(res,
                   c(sec.hrules.at,sectsep.at),
                   c(sec.hrules,   sectseps)
                   )
    
    if(l.headers > 1 || l.headers > 0 && has.eq.headers){
        n.rules <- l.headers - 1
        if(has.eq.headers)
            n.rules <- n.rules + 1
        hi.rules.at <- seq.int(n.rules)
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
        signif.symbols <- format_signif_latex(signif.symbols,
                                              signif.template,
                                              dec=LaTeXdec,
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

format_signif_latex <- function(syms,tmpl,width,dec="."){
    title <- tmpl[1]
    clps <- tmpl[3]
    tmpl <- tmpl[2]
    res <- title
    empty.title <- paste(rep(" ",nchar(title)),collapse="")
    
    ns <- length(syms)
    dotrepl <- paste0("{",dec,"}")
    for(i in 1:ns){
        sym <- names(syms)[i]
        thrsh <- unname(syms[i])
        thrsh <- gsub(".",dotrepl,thrsh,fixed=TRUE)
        res.i <- sub("$sym",sym,tmpl,fixed=TRUE)
        res.i <- sub("$val",thrsh,res.i,fixed=TRUE)
        if(i < ns)
            res.i <- paste0(res.i,clps)
        len <- length(res)
        res.l <- res[len]
        n.res.l <- nchar(res.l)
        n.res.i <- nchar(res.i)
        if(n.res.l+n.res.i <= width)
            res[len] <- paste0(res.l,res.i)
        else
            res <- c(res,paste0(empty.title,res.i))
    }
    res
}
