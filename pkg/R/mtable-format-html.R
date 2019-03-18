mtable_format_stdstyle <- c(
  "padding-top"="1px",
  "padding-bottom"="1px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px",
  "border-style"="none",
  "border-width"="0px"
)

format_html.memisc_mtable <- function(x,
                               interaction.sep = NULL,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               margin="2ex auto",
                               sig.notes.style=c(width="inherit"),
                               ...){
  x <- preformat_mtable(x)
  res <- pf_mtable_format_html(x,interaction.sep=interaction.sep,
                        toprule=toprule,midrule=midrule,bottomrule=bottomrule,
                        split.dec=split.dec,style=style,margin=margin,
                        sig.notes.style=sig.notes.style,...)
  as.character(res)
}

mtable_format_html <- function(x,
                               interaction.sep = NULL,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               margin="2ex auto",
                               sig.notes.style=c(width="inherit"),
                               ...)
    pf_mtable_format_html(preformat_mtable(x),
                          interaction.sep=interaction.sep,
                          toprule=toprule,
                          midrule=midrule,
                          bottomrule=bottomrule,
                          split.dec=split.dec,
                          style=style,
                          margin=margin,
                          sig.notes.style=sig.notes.style,
                          ...)

pf_mtable_format_html <- function(x,
                               interaction.sep = NULL,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               margin="2ex auto",
                               sig.notes.style=c(width="inherit"),
                               ...
){
  
    firstcol <- c("padding-left"="0.3em")
    toprule <- c("border-top"=paste0(midrule,"px solid"))
    bottomrule <- c("border-bottom"=paste0(midrule,"px solid"))
    midrule_above <- c("border-top"=paste0(midrule,"px solid"))
    midrule <- c("border-bottom"=paste0(midrule,"px solid"))
    align.right <- c("text-align"="right")  
    align.left <- c("text-align"="left")  
    align.center <- c("text-align"="center")
    lrpad <- c("padding-left"="0.3em","padding-right"="0.3em")
    row_style <- c("border-style"="none")
    table_style <- c("border-collapse"="collapse" ,"border-style"="none")
    
    if(!length(interaction.sep)){
        if(getOption("html.use.ampersand",FALSE))
            interaction.sep <- " &times; "
        else 
            interaction.sep <- " \u00d7 "
    }

    colsep <- ""
    rowsep <- "\n"

    pt <- x$parmtab
    sst <- x$summary.stats
    sh <- x$sect.headers
    leaders <- x$leaders
    headers <- x$headers
    l.headers <- length(headers)
    l.leaders <- length(leaders)

    ncols <- sapply(pt[1,,drop=FALSE],ncol)

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
        span.j <- unique(unlist(lapply(sh.j,attr,"span")))
        if(is.null(span.j)|| !span.j) span.j <- 1
        
        for(i in 1:length(pt.j)){

            pt.ij <- pt.j[[i]]
            dm.ij <- dim(pt.ij)
            
            if(getOption("html.use.ampersand",FALSE))
                pt.ij[] <- gsub("-","&minus;",pt.ij[],fixed=TRUE)
            else
                pt.ij[] <- gsub("-","\u2212",pt.ij[],fixed=TRUE)

            if(split.dec){
                pt.ij <- spltDec(pt.ij)
                pt.ij <- gsub("([*]+)","<span class=\"signif.symbol\">\\1</span>",pt.ij)
                pt.ij <- html_td_spltDec(pt.ij, style=css(style))
            }
            else
                pt.ij[] <- html_td(pt.ij,style=css(style),vectorize=TRUE)
            dim(pt.ij) <- dm.ij
            
            if(need.sh[[i]]){
                sh.ij <- sh.j[[i]]
                if(!length(sh.ij)){
                    sh.ij <- ""
                    hstyle <- upd_vect(style,align.center)
                }
                else
                    hstyle <- upd_vect(style,align.center,midrule_above)
                colspan <- span.j
                if(split.dec)
                    colspan <- 3*colspan
                sh.ij <- html_td(sh.ij,colspan=colspan,style=css(hstyle),vectorize=TRUE)
                pt.ij <- rbind(sh.ij,pt.ij)
            }
            pt.j[[i]] <- pt.ij
        }
        pt.j <- do.call(rbind,pt.j)

        if(length(sst)){
            sst.j <- sst[[j]]
            if(getOption("html.use.ampersand",FALSE))
                sst.j <- gsub("-","&minus;",sst.j,fixed=TRUE)
            else
                sst.j <- gsub("-","\u2212",sst.j,fixed=TRUE)
            sst.j <- colexpand(sst.j,ncol.j)
            dm.ij <- dim(sst.j)
            if(split.dec){
                sst.j <- spltDec(sst.j)
                sst.j <- html_td_spltDec(sst.j,style=css(style))
            }
            else
                sst.j <- html_td(sst.j,style=css(style),vectorize=TRUE)
            dim(sst.j) <- dm.ij
            pt.j <- rbind(pt.j,sst.j)
        }

        pt.j <- apply(pt.j,1,as.html_group)
        
        res <- cbind(res,pt.j)
    }

    if(l.leaders){

        for(i in 1:l.leaders){
            if(need.sh[i]){
                leaders.i <- leaders[[i]]
                leaders[[i]] <- c(list(structure("",span=1)),leaders.i)
            }
        }
        
        leaders <- lapply(leaders,ldxp)
        leaders <- do.call(rbind,leaders)

        lstyle <- upd_vect(style,align.left,firstcol)
        leaders <- html_td(leaders,vectorize=TRUE,style=css(lstyle))
        
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,as.html_group)

    if(l.headers){
        for(k in 1:l.headers){
            headers.k <- headers[[k]]
            hspan.k <- sapply(headers.k,attr,"span")
            if(k == l.headers)
                hstyle <- upd_vect(style,align.center)
            else
                hstyle <- upd_vect(style,align.center,midrule)
            if(split.dec)
                hspan.k <- hspan.k*3
            headers.k <- Map(html_td,headers.k,colspan=hspan.k,MoreArgs=list(style=css(hstyle)))
            if(l.leaders){
                hlstyle <- upd_vect(style,align.left)
                lheader.k <- html_td("",colspan=1,style=css(hlstyle))
                headers.k <- c(list(lheader.k),headers.k)
            }
            headers[[k]] <- headers.k
        }
        headers <- lapply(headers,as.html_group)
        res <- c(headers,res)
    }

    
    res[[1]] <- lapply(res[[1]],setStyle,toprule)
    n <- length(res)
    res[[n]] <- lapply(res[[n]],setStyle,bottomrule)


    sect.at <- integer()
    csum <- 1
    for(i in 1:nrow(pt)){
        if(need.sh[i]){
            csum <- csum + 1
        }
        sect.at <- c(sect.at,csum)
        csum <- csum + nrow(pt[[i,1]])
    }
    if(length(sst) && any(sapply(sst,length)>0))
        sect.at <- c(sect.at,csum)
    if(l.headers)
        sect.at <- c(sect.at + l.headers)
    #browser()
    for(i in sect.at)
        res[[i]] <- lapply(res[[i]],setStyle,midrule_above)

    signif.symbols <- x$signif.symbols
    if(length(signif.symbols)){
        signif.template <- getOption("signif.symbol.print.template",
                                     signif.symbol.print.default.template)
        signif.symbols <- format_signif_print(signif.symbols,
                                              signif.template,
                                              width=72)

        if(split.dec)
            totspan <- ncols * 3
        else
            totspan <- ncols
        if(l.leaders)
            totspan <- totspan + 1
        
        signif.symbols <- html_p(signif.symbols,style=css(sig.notes.style))
        signif.symbols <- html_td(signif.symbols,style=css(style),colspan=totspan)
        res <- c(res,list(signif.symbols))
    }
    
    res <- html_tr(res,vectorize=TRUE,style=as.css(row_style))
    
    if(length(margin))
        table_style <- c(table_style,margin=margin)
    res <- html_table(res,class="mtable",style=as.css(table_style))

    return(res)

}

get_colspan <- function(x)x$attributes$colspan
