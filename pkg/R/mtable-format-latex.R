mtable_format_latex <- function(x,...)
    pf_mtable_format_latex(preformat_mtable(x),...)


pf_mtable_format_latex <- function(x,
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=min(3,getOption("digits")),
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          interaction.sep = " $\\times$ ",
          sdigits=min(1,ddigits),
          force.names = FALSE,
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
    
    for(j in 1:ncol(pt)){
        
        pt.j <- pt[,j]
        sh.j <- sh[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        span.j <- unique(sapply(sh.j,attr,"span"))

        if(is.numeric(span.j))
            pt.j <- lapply(pt.j,cols_folded,span.j,sep=colsep)

        for(i in 1:length(pt.j)){
            pt.ij <- pt.j[[i]]
            pt.ij <- centerAt(pt.ij,at=".",integers="dot")
            if(!useDcolumn)
                pt.ij[] <- paste0("$",pt.ij,"$")
            if(need.sh[[i]]){
                sh.ij <- sh.j[[i]]
                if(!length(sh.ij)) sh.ij <- ""
                sh.ij <- set_length(sh.ij,ncol.j/span.j)
                sh.ij <- paste0("\n\\multicolumn{",span.j,"}{c}{",sh.ij,"}")
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
            sst.j <- centerAt(sst.j,at=".",integers="dot")
            sst.j <- colexpand(sst.j,ncol.j)
            sst.j <- apply(sst.j,1,paste,collapse=colsep)
            pt.j <- c(pt.j,sst.j)
        }

        if(length(headers)){
            header.j <- headers[[j]]
            if(length(header.j)){
                hspan.j <- attr(header.j,"span")
                stopifnot(hspan.j==ncol.j) # TODO: deal with extra-wide heders
            }
            else {
                header.j <- ""
                hspan.j <- ncol.j
            }
            header.j <- paste0("\n\\multicolumn{",hspan.j,"}{c}{",header.j,"}")
            pt.j <- c(header.j,pt.j)
        }
        
        res <- cbind(res,pt.j)
    }

    if(length(leaders)){

        for(i in 1:length(leaders)){
            if(need.sh[i]){
                leaders.i <- leaders[[i]]
                leaders[[i]] <- c(list(structure("",span=1)),leaders.i)
            }
        }
        
        if(length(headers))
            leaders <- c(list(headers=list(structure("",span=1))),leaders)
# TODO: deal with multiline headers
        leaders <- lapply(leaders,ldxp)
        leaders <- do.call(rbind,leaders)
        leaders <- gsub(" x ",interaction.sep,leaders,fixed=TRUE)
        leaders <- format(leaders,justify="left")
        leaders <- gsub("$\\"," $\\",leaders,fixed=TRUE)
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,paste,collapse=modelsep)
    res <- paste0(res,tabcr)

    sect.at <- integer()
    csum <- 0
    for(i in 1:nrow(pt)){
        if(need.sh[i]){
            csum <- csum + 1
            sect.at <- c(sect.at,csum)
        }
        csum <- csum + nrow(pt[[i,1]])
        sect.at <- c(sect.at,csum)
    }
    if(length(headers))
        sect.at <- c(1,sect.at + 1)
    res <- .insert(res,sect.at,list(midrule))
    
    res <- c(toprule,res,bottomrule)

    tab.spec <- character()
    
    if(length(leaders))
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



