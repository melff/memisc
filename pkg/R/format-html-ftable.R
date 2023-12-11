ftable_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="3px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px",
  "border-style"="none",
  "border-width"="0"
)

style_ftab_global <- function(id=NULL,class=NULL,style=ftable_format_stdstyle,margin="2ex auto") {

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

style_ftab_rule <- function(id=NULL,class=NULL,top=FALSE,bottom=FALSE,rulewidth=1,rows=NULL,firstcol=NULL){
    if(length(rows)){
        class_or_id <- class_id_selector(id=id,class=class)
        rulewidth <- paste0(rulewidth,"px")
        selector <- character(0)
        if(length(firstcol)) {
            selector_tmpl <- "table<<class-or-id>> tr:nth-child(<<row>>) td:nth-child(n+<<firstcol>>)"
            for(row in rows){
                selector <- c(selector,
                              fillin(selector_tmpl,c("class-or-id"=class_or_id,row=row,firstcol=firstcol)))
            }
        }
        else {
            selector_tmpl <- "table<<class-or-id>> tr:nth-child(<<row>>)"
            for(row in rows){
                selector <- c(selector,
                              fillin(selector_tmpl,c("class-or-id"=class_or_id,row=row)))
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

style_ftab_cols <- function(id=NULL,class=NULL,cols=NULL,style=""){
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

style_ftab_header <- function(id=NULL,class=NULL,style=""){
    class_or_id <- class_id_selector(id=id,class=class)
    selector_tmpl <- "table<<class-or-id>> td:nth-child(n of .header)"
    selector <- fillin(selector_tmpl,c("class-or-id"=class_or_id,col=col))
    paste0("\n",selector,"{",style,"}")
}

ftab_html_env <- new.env()
ftab_html_env$counter <- 1

format_html.ftable <- function(x,
                               show.titles=TRUE,
                               digits=0,
                               format="f",
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               style=ftable_format_stdstyle,
                               margin="2ex auto",
                               ...){
    
    row.vars <- attr(x,"row.vars")
    col.vars <- attr(x,"col.vars")
    n.row.vars <- length(row.vars)
    n.col.vars <- length(col.vars)
    n <- nrow(x)
    m <- ncol(x)
    d <- digits
    digits <- integer(m)
    digits[] <- d
    
    fo <- format
    format <- integer(m)
    format[] <- fo
    
    body <- array("",dim=dim(x))
    for(i in seq(along=digits)) {
    #print(digits[i])
        body[,i] <- formatC(x[,i],digits=digits[i],format=format[i])
    }
    
    body <- array(trimws(body),dim=dim(x))
    if(getOption("html.use.ampersand",FALSE))
        body[] <- gsub("-","&minus;",body[],fixed=TRUE)
    else
        body[] <- gsub("-","\u2212",body[],fixed=TRUE)
    
    dot_cols <- integer(0)

    if(split.dec){
        tmp <- spltDec(body)
        body <- html_td_spltDec(tmp)
        dim(body) <- dim(x)
        colspan <- 3L
        dot_cols <- (1:m-1)*3 + 2
    }
    else {
        body <- html_td(body,vectorize=TRUE)
        dim(body) <- dim(x)
        colspan <- 1L
    }
    
    leaders <- array(list(),dim=c(n,n.row.vars))
    if(show.titles)
        leaders <- cbind(leaders,"")

    mm <- 1
    for(j in rev(1:n.row.vars)){
        rv <-row.vars[[j]]
        nrv <- length(rv)
        nn <- n/mm
        i <- (1:nn)*mm - mm + 1
        leaders[i,j] <- rv
        mm <- mm*nrv
    }
    for(i in 1:n){
        leaders[i,] <- html_td(leaders[i,],
                               vectorize=TRUE) 
    }
    nl <- ncol(leaders)
    body <- cbind(leaders,body)
    nr <- nrow(body)

    body <- as.html_group(apply(body,1,html_tr))
    
    header <- list()
    mm <- 1
    for(i in rev(1:n.col.vars)){
        cv <- col.vars[[i]]
        ncv <- length(cv)
        if(split.dec)
            attribs <- list(colspan=mm*3)
        else
            attribs <- list(colspan=mm)
        mm <- mm*ncv
        cv <- rep(cv,m%/%mm)
        
        if(show.titles){
            if(n.col.vars == 1){
                htmp1 <- html_td(c(names(row.vars),""),
                                 vectorize=TRUE)
            }
            else {
                if(i == n.col.vars){
                    htmp1 <- html_td(c(names(row.vars),paste0(names(col.vars)[i],":")),
                                     vectorize=TRUE)
                }
                else
                    htmp1 <- html_td(c(rep("",n.row.vars),paste0(names(col.vars)[i],":")),
                                     vectorize=TRUE)
            }      
        }
        else 
            htmp1 <- html_td(rep("",ncol(leaders)),
                             vectorize=TRUE)
        attribs$class <- "header"
        htmp2 <- setAttribs(html_td(cv,vectorize=TRUE),attribs)
        header <- c(list(c(htmp1,htmp2)),header)
    }
    if(show.titles && n.col.vars == 1){
        if(nzchar(names(col.vars))){
            htmp1 <- html_td(rep("",ncol(leaders)),
                             vectorize=TRUE)
            colspan <- ncol(x)
            if(split.dec) 
                colspan <- colspan*3
            attribs <- list(colspan=colspan)
            attribs$class <- "header"
            htmp2 <- setAttribs(html_td(names(col.vars),vectorize=TRUE),attribs)
            header <- c(list(c(htmp1,htmp2)),header)
        }
    }
    header <- html_tr(header,vectorize=TRUE)
    nh <- length(header)

    ftab_id <- paste0("ftable-",ftab_html_env$counter)
    res <- html_table(c(header,body),class="ftable",id=ftab_id)

    ftab_html_env$counter <- ftab_html_env$counter + 1

    style_global <- style_ftab_global(id=ftab_id,style=style,margin=margin)
    style_toprule <- style_ftab_rule(id=ftab_id,rulewidth=toprule,top=TRUE,
                                       rows=1)
    style_bottomrule <- style_ftab_rule(id=ftab_id,rulewidth=bottomrule,bottom=TRUE,
                                          rows=nr+nh)
    style_midrule <- style_ftab_rule(id=ftab_id,rulewidth=midrule,bottom=TRUE,
                                          rows=nh)

    style_content <- paste(
        style_global,
        style_toprule,
        style_midrule,
        style_bottomrule,
        sep="\n"
    )

    if(nh > 1){
        style_grouprule <- style_ftab_rule(id=ftab_id,rulewidth=midrule,bottom=TRUE,
                                           firstcol=nl+1,
                                           rows=1:(nh-1))
        style_content <- paste(
            style_content,
            style_grouprule,
            sep="\n"
        )
    }

    if(split.dec){
        dot_style <- "
         padding-left: 0px;
         padding-right: 0px;
         text-align: center; 
         width: .2ex;
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
        dot_cols <- dot_cols + nl - 1
        style_dots <- style_df_cols(id=ftab_id,cols=dot_cols+1,style=dot_style)
        style_before_dots <- style_df_cols(id=ftab_id,cols=dot_cols,style=before_dot_style)
        style_behind_dots <- style_df_cols(id=ftab_id,cols=dot_cols+2,style=behind_dot_style)
        style_content <- paste(style_content,
                               style_dots,
                               style_before_dots,
                               style_behind_dots,sep="\n")
    }
    header_style <- "
         padding-left: 0.5ex;
         padding-right: 0.5ex;
         text-align: center; 
         /*background-color: gray;*/
        "
    style_header <- style_ftab_header(id=ftab_id,style=header_style)
    style_content <- paste(style_content,
                           style_header,
                           "\n")

    style_element <- html("style",style_content,linebreak=TRUE)
    res <- html_group(style_element,res)
    res <- as.character(res)
    return(res)
}

