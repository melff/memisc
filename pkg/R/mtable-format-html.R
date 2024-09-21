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

fillin <- function(template,substitutions){
    res <- template
    for(n in names(substitutions)){
        pat <- paste0("<<",n,">>")
        subst <- substitutions[n]
        res <- gsub(pat,subst,res)
    }
    res
}

class_id_selector <- function(id=NULL,class=NULL){
    class_or_id <- ""
    if(length(class)){
       class_or_id <- paste0(".",paste0(class,collapse="."))   
    } 
    if(length(id)){
        id <- paste0("#",id[1])
        class_or_id <- paste0(class_or_id,id)
    }
    class_or_id
}

style_mtable_global <- function(id=NULL,class=NULL,style=mtable_format_stdstyle,margin="2ex auto") {

    table_tmpl <- "
      table<<class-or-id>> {
         border-collapse: collapse; border-style: none; margin: <<margin>>;
      }"
    table_tr_tmpl <- "
      table<<class-or-id>> tr {
          border-style: none;
      }"
    table_td_tmpl <- "
      table<<class-or-id>> td {
        padding-top: <<padding-top>>; 
        padding-bottom: <<padding-bottom>>; 
        padding-left: <<padding-left>>; 
        padding-right: <<padding-right>>; 
        margin: 0px; 
        margin-top: <<margin-top>>;
        margin-bottom: <<margin-bottom>>;
        border-style: <<border-style>>; 
        border-width: <<border-width>>; 
      }"

    class_or_id <- class_id_selector(id=id,class=class)

    table_style <- fillin(table_tmpl,
                          c("class-or-id"=class_or_id,
                            margin=margin))
    table_tr_style <- fillin(table_tr_tmpl,
                             c("class-or-id"=class_or_id))
    table_td_style <- fillin(table_td_tmpl,
                             c("class-or-id"=class_or_id,
                               style))
    paste(table_style,table_tr_style,table_td_style)
}

style_mtable_rule <- function(id=NULL,class=NULL,top=FALSE,bottom=FALSE,rulewidth=1,rows=NULL){
    if(length(rows)){
        class_or_id <- class_id_selector(id=id,class=class)
        rulewidth <- paste0(rulewidth,"px")
        selector_tmpl <- "table<<class-or-id>> tr:nth-child(<<row>>)"
        selector <- character(0)
        for(row in rows){
            selector <- c(selector,
                          fillin(selector_tmpl,c("class-or-id"=class_or_id,row=row)))
        }
        selector <- paste(selector,collapse=",\n")
        style_tmpl <- "<<selector>>{"
        if(top){
            style_tmpl <- paste(style_tmpl,
                                "     border-top: <<rulewidth>> solid;",
                                sep="\n")
        }
        if(bottom){
            style_tmpl <- paste(style_tmpl,
                                "     border-bottom: <<rulewidth>> solid;",
                                sep="\n")
        }
        style_tmpl <- paste(style_tmpl,"}",sep="\n")

        fillin(style_tmpl,c(selector=selector,rulewidth=rulewidth))
    } else ""
}

style_mtable_cmidrule <- function(id=NULL,class=NULL,top=FALSE,bottom=FALSE,rulewidth=0.5,cols=NULL,rows=NULL){
    if(length(cols) && length(rows)){
        class_or_id <- class_id_selector(id=id,class=class)
        rulewidth <- paste0(rulewidth,"px")
        selector_tmpl <- "table<<class-or-id>> tr:nth-child(<<row>>) td:nth-child(<<col>>)"
        selector <- character(0)
        for(row in rows){
            for(col in cols){
                selector <- c(selector,
                              fillin(selector_tmpl,c("class-or-id"=class_or_id,row=row,col=col)))
            }
        }
        selector <- paste(selector,collapse=",\n")
        style_tmpl <- "<<selector>>{"
        if(top){
            style_tmpl <- paste(style_tmpl,
                                "     border-top: <<rulewidth>> solid;",
                                sep="\n")
        }
        if(bottom){
            style_tmpl <- paste(style_tmpl,
                                "     border-bottom: <<rulewidth>> solid;",
                                sep="\n")
        }
        style_tmpl <- paste(style_tmpl,"}",sep="\n")

        fillin(style_tmpl,c(selector=selector,rulewidth=rulewidth))
    } else ""
}



style_mtable_cols <- function(id=NULL,class=NULL,cols=NULL,style=""){
    if(length(cols)){
        class_or_id <- class_id_selector(id=id,class=class)
        selector_tmpl <- "table<<class-or-id>> td:nth-child(<<col>>)"
        selector <- character(0)
        for(col in cols){
            selector <- c(selector,
                          fillin(selector_tmpl,c("class-or-id"=class_or_id,col=col)))
        }
        selector <- paste(selector,collapse=",\n")
        paste0(selector,"{",style,"}")
    }
}

style_mtable_header <- function(id=NULL,class=NULL,style=""){
    class_or_id <- class_id_selector(id=id,class=class)
    selector_tmpl <- "table<<class-or-id>> td:nth-child(n of .header)"
    selector <- fillin(selector_tmpl,c("class-or-id"=class_or_id,col=col))
    paste0("\n",selector,"{",style,"}")
}



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

mtable_html_env <- new.env()
mtable_html_env$counter <- 1

pf_mtable_format_html <- function(x,
                               interaction.sep = NULL,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=mtable_format_stdstyle,
                               margin="2ex auto",
                               sig.notes.style=c(width="inherit"),
                               show.parmtypes = nrow(x$parmtab) > 1,
                               ...
){
  
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
    leaders <- x$leaders
    headers <- x$headers
    eq.headers <- x$eq.headers
    outtypes <- x$outtypes
    l.headers <- length(headers)
    l.leaders <- length(leaders)

    ncols <- sapply(pt[1,,drop=FALSE],ncol)

    res <- NULL
    has.eq.headers <- length(eq.headers) > 0

    total_hdr_lines <- 1
    total_pt_lines <- 0
    total_sum_lines <- 0

    dot_cols <- integer(0)
    col_sum <- 0

    for(j in 1:ncol(pt)){
        
        name.j <- colnames(pt)[j]
        pt.j <- pt[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        
        for(i in 1:length(pt.j)){

            pt.ij <- pt.j[[i]]
            ot.ij <- outtypes[i,j]
            dm.ij <- dim(pt.ij)
            
            if(ot.ij == "num"){
                if(getOption("html.use.ampersand",FALSE))
                    pt.ij[] <- gsub("-","&minus;",pt.ij[],fixed=TRUE)
                else
                    pt.ij[] <- gsub("-","\u2212",pt.ij[],fixed=TRUE)

                if(split.dec){
                    pt.ij <- spltDec(pt.ij)
                    pt.ij <- gsub("([*]+)","<span class=\"signif.symbol\">\\1</span>",pt.ij)
                    pt.ij <- html_td_spltDec(pt.ij,add_CSS=FALSE)
                }
                else
                    pt.ij[] <- html_td(pt.ij,vectorize=TRUE)
            } else {
                if(split.dec)
                    pt.ij[] <- html_td(pt.ij,colspan=3,vectorize=TRUE)
                else 
                    pt.ij[] <- html_td(pt.ij,vectorize=TRUE)
            }
            dim(pt.ij) <- dm.ij
            if(show.parmtypes){
                spaces <- rep(" ",ncol(pt.ij))
                spaces <- html_td(spaces,vectorize=TRUE)
                pt.ij <- rbind(spaces,pt.ij)
            }
            pt.j[[i]] <- pt.ij
        }
        pt.j <- do.call(rbind,pt.j)
        if(split.dec){
            dot_cols_cur <- (1:ncol(pt.j)-1)*3 + 2
            dot_cols <- c(dot_cols, col_sum + dot_cols_cur)
            col_sum <- col_sum + ncol(pt.j)*3
        }

        if(has.eq.headers){
           eq.header.j <- eq.headers[[name.j]]
            n.eq.j <- length(eq.header.j)
            eq.span <- ncol.j/n.eq.j
            if(split.dec)
                eq.span <- eq.span*3
            eq.header.j <- html_td(eq.header.j,colspan=eq.span,vectorize=TRUE,class="header")
            # pt.j <- rbind(eq.header.j,pt.j)
            #total_hdr_lines <- max(total_hdr_lines,1+n.eq.j)
           eq.headers[[name.j]] <- eq.header.j
        }
        total_pt_lines <- max(total_pt_lines,1+nrow(pt.j))

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
                sst.j <- html_td_spltDec(sst.j,add_CSS=FALSE)
            }
            else
                sst.j <- html_td(sst.j,vectorize=TRUE)
            dim(sst.j) <- dm.ij
            pt.j <- rbind(pt.j,sst.j)
            total_sum_lines <- max(total_sum_lines,nrow(sst.j))
        }

        pt.j <- apply(pt.j,1,as.html_group)
        
        res <- cbind(res,pt.j)
    }

    if(l.leaders){
        leaders <- lapply(leaders,ldxp)
        if(show.parmtypes){
            parmtypes <- rownames(x$parmtab)
            for(p in parmtypes){
                leaders[[p]] <- rbind(p,leaders[[p]])
            }
        }
        leaders <- do.call(rbind,leaders)

        leaders <- html_td(leaders,vectorize=TRUE)
        
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,as.html_group)

    if(l.headers){
        for(k in 1:l.headers){
            headers.k <- headers[[k]]
            hspan.k <- sapply(headers.k,attr,"span")
            if(split.dec)
                hspan.k <- hspan.k*3
            headers.k <- Map(html_td,headers.k,colspan=hspan.k,class="header")
            if(l.leaders){
                lheader.k <- html_td("",colspan=1,class="header")
                headers.k <- c(list(lheader.k),headers.k)
            }
            headers[[k]] <- headers.k
        }
        headers <- lapply(headers,as.html_group)
        if(has.eq.headers) {
            if(l.leaders){
                lheader.e <- html_td("",colspan=1,class="header")
                tmp <- unlist(eq.headers,recursive=FALSE)
                eq.headers <- as.html_group(c(list(lheader.e),tmp))
            }
            headers[[l.headers + 1]] <- eq.headers
        }
        res <- c(headers,res)
    }
    else if(has.eq.headers) {
        if(l.leaders){
            lheader.e <- html_td("",colspan=1,class="header")
            tmp <- unlist(eq.headers,recursive=FALSE)
            eq.headers <- as.html_group(c(list(lheader.e),tmp))
        }
        res <- c(list(eq.headers), res)
    }

    csum <- l.headers

    sect.at <- integer()
    for(i in 1:nrow(pt)){
        sect.at <- c(sect.at,csum)
        csum <- csum + nrow(pt[[i,1]])
        if(nrow(pt) > 1)
            csum <- csum + 1
    }
    if(length(sst) && any(sapply(sst,length)>0))
        sect.at <- c(sect.at,csum)
    if(has.eq.headers)
        sect.at <- sect.at + 1
    lines_total <- length(res)

    signif.symbols <- x$signif.symbols
    if(length(signif.symbols)){
        signif.template <- getOption("signif.symbol.print.template",
                                     signif.symbol.print.default.template)
        signif.symbols <- format_signif_print(signif.symbols,
                                              signif.template,
                                              width=72)
        signif.symbols <- gsub("<","&lt;",signif.symbols)
        signif.symbols <- gsub(">","&gt;",signif.symbols)
        
        if(split.dec)
            totspan <- sum(ncols) * 3
        else
            totspan <- sum(ncols)
        if(l.leaders)
            totspan <- totspan + 1
        
        signif.symbols <- html_p(signif.symbols)
        signif.symbols <- html_td(signif.symbols,colspan=totspan)
        res <- c(res,list(signif.symbols))
    }
    
    res <- html_tr(res,vectorize=TRUE)

    mtable_id <- paste0("mtable-",mtable_html_env$counter)
    res <- html_table(res,id=mtable_id)

    mtable_html_env$counter <- mtable_html_env$counter + 1

    #midrule_lines <- total_hdr_lines
    #midrule_lines <- c(midrule_lines,total_hdr_lines + total_pt_lines)
    midrule_lines <- sect.at

    style_global <- style_mtable_global(id=mtable_id,style=style,margin=margin)
    style_toprule <- style_mtable_rule(id=mtable_id,rulewidth=toprule,top=TRUE,
                                       rows=1)
    style_bottomrule <- style_mtable_rule(id=mtable_id,rulewidth=bottomrule,bottom=TRUE,
                                          rows=lines_total)
    style_midrule <- style_mtable_rule(id=mtable_id,rulewidth=midrule,bottom=TRUE,
                                          rows=midrule_lines)


    if(l.headers > 0 && has.eq.headers) {
        if(l.leaders) cmod_fst <- 2 else cmod_fst <- 1
        last.header <- headers[[l.headers]]
        cmod_lst <- length((last.header))
        cmid_cols <- cmod_fst:cmod_lst
        style_mtable_cmidrule <- style_mtable_cmidrule(id=mtable_id,bottom=TRUE,
                                                       rows=l.headers,cols=cmid_cols)
    }
    else style_mtable_cmidrule <- NULL

    style_content <- paste(
        style_global,
        style_toprule,
        style_mtable_cmidrule,
        style_midrule,
        style_bottomrule,
        sep="\n"
    )
    if(split.dec){
        dot_style <- "
         padding-left: 0px;
         padding-right: 0px;
         text-align: center; 
         /*background-color: blue;*/
        "
        before_dot_style <- "
         padding-left: 0.5ex;
         padding-right: 0px;
         text-align: right; 
         /*background-color: red;*/
        "
        behind_dot_style <- "
         padding-left: 0px;
         padding-right: 0.5ex;
         text-align: left; 
         /*background-color: green;*/
        "
        
        style_dots <- style_mtable_cols(id=mtable_id,cols=dot_cols+1,style=dot_style)
        style_before_dots <- style_mtable_cols(id=mtable_id,cols=dot_cols,style=before_dot_style)
        style_behind_dots <- style_mtable_cols(id=mtable_id,cols=dot_cols+2,style=behind_dot_style)
        style_content <- paste(style_content,
                               style_dots,
                               style_before_dots,
                               style_behind_dots)
    }
    header_style <- "
         padding-left: 0.5ex;
         padding-right: 0.5ex;
         text-align: center; 
         /*background-color: gray;*/
        "
    style_header <- style_mtable_header(id=mtable_id,style=header_style)
    style_content <- paste(style_content,
                           style_header,
                           "\n")

    style_element <- html("style",style_content,linebreak=FALSE)

    res <- html_group(style_element,res)

    return(res)

}

get_colspan <- function(x)x$attributes$colspan
