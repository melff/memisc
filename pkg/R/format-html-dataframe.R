df_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="3px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px",
  "border-style"="none",
  "border-width"="0px"
)


style_df_global <- function(id=NULL,class=NULL,style=df_format_stdstyle,margin="2ex auto") {

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

style_df_rule <- function(id=NULL,class=NULL,top=FALSE,bottom=FALSE,rulewidth=1,rows=NULL){
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

style_df_cols <- function(id=NULL,class=NULL,cols=NULL,style=""){
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

style_df_header <- function(id=NULL,class=NULL,style=""){
    class_or_id <- class_id_selector(id=id,class=class)
    selector_tmpl <- "table<<class-or-id>> td:nth-child(n of .header)"
    selector <- fillin(selector_tmpl,c("class-or-id"=class_or_id,col=col))
    paste0("\n",selector,"{",style,"}")
}

df_html_env <- new.env()
df_html_env$counter <- 1

format_html.data.frame <- function(x,
                                   toprule=2,midrule=1,bottomrule=2,
                                   split.dec=TRUE,
                                   row.names=TRUE,
                                   digits=getOption("digits"),
                                   format="f",
                                   style=df_format_stdstyle,
                                   margin="2ex auto",
                                   ...){

    
    colsep <- ""
    rowsep <- "\n"
    
    n <- nrow(x)
    m <- ncol(x)
    d <- digits
    is.factor <- sapply(x,is.factor)
    is.int <- sapply(x,is.integer) & !is.factor
    is.num <- sapply(x,is.numeric) & !(is.int | is.factor)
    m.num <- sum(is.num)
    digits <- integer(m.num)
    digits[] <- d
    fdigits <- integer(m)
    fdigits[is.num] <- digits
    fo <- format
    format <- character(m)
    format[is.num] <- fo  
    
    dot_cols <- integer(0)
    col_sum <- 0

    colspan <- integer(0)
    body <- matrix(nrow=nrow(x),ncol=0)
    for(i in 1:m) {
        tmp <- x[[i]]
        dim.x.i <- dim(tmp)
        ncol.tmp <- if(length(dim.x.i)) ncol(tmp) else 1
        if(is.int[i]){
            tmp <- formatC(tmp,format="d")
            col <- html_td(tmp,vectorize=TRUE)
            colspan <- c(colspan,ncol.tmp)
            col_sum <- col_sum + ncol.tmp
        }
        else if(is.num[i]){
            tmp <- formatC(tmp,digits=fdigits[i],format=format[i])
            if(split.dec){
                tmp <- spltDec(tmp)
                col <- html_td_spltDec(tmp)
                dot_cols_cur <- (1:ncol.tmp-1)*3 + 2
                dot_cols <- c(dot_cols, col_sum + dot_cols_cur)
                colspan <- c(colspan,3L*ncol.tmp)
                col_sum <- col_sum + 3L*ncol.tmp
            }
            else{
                col <- html_td(tmp,vectorize=TRUE)
                colspan <- c(colspan,ncol.tmp)
                col_sum <- col_sum + ncol.tmp
            }
        }
        else {
            tmp <- as.character(tmp)
            col <- html_td(tmp,vectorize=TRUE)
            colspan <- c(colspan,ncol.tmp)
            col_sum <- col_sum + ncol.tmp
        }
        dim(col) <- dim.x.i
        body <- cbind(body,col)
    }
    #browser()
    if(row.names){
        tmp <- rownames(x)
        ldr <- html_td(tmp,vectorize=TRUE)
        body <- cbind(ldr,body)
    }
    
    body <- apply(body,1,html_tr)
    
    hdr <- colnames(x)
    if(row.names) {
        hdr <- c("",hdr)
        colspan <- c(1L,colspan)
    }
    
    hdr <- html_td(hdr,vectorize=TRUE,class="header")
    hdr[] <- mapply(setAttribs,hdr,colspan=colspan,SIMPLIFY=FALSE)
    hdr <- html_tr(hdr)
    
    res <- c(list(hdr),body)

    df_id <- paste0("data-frame-",df_html_env$counter)
    res <- html_table(res,id=df_id)

    df_html_env$counter <- df_html_env$counter + 1

    style_global <- style_df_global(id=df_id,style=style,margin=margin)
    style_toprule <- style_df_rule(id=df_id,rulewidth=toprule,top=TRUE,
                                       rows=1)
    style_bottomrule <- style_df_rule(id=df_id,rulewidth=bottomrule,bottom=TRUE,
                                          rows=nrow(x)+1)
    style_midrule <- style_df_rule(id=df_id,rulewidth=midrule,bottom=TRUE,
                                          rows=1)

    style_content <- paste(
        style_global,
        style_toprule,
        style_midrule,
        style_bottomrule,
        sep="\n"
    )

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
        style_dots <- style_df_cols(id=df_id,cols=dot_cols+1,style=dot_style)
        style_before_dots <- style_df_cols(id=df_id,cols=dot_cols,style=before_dot_style)
        style_behind_dots <- style_df_cols(id=df_id,cols=dot_cols+2,style=behind_dot_style)
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
    style_header <- style_df_header(id=df_id,style=header_style)
    leader_style <- "
         padding-left: 0.5ex;
         padding-right: 0px;
         text-align: right; 
         /*background-color: red;*/
        "
    style_leader <- style_mat_cols(id=df_id,cols=1,style=leader_style)    
    style_content <- paste(style_content,
                           style_header,
                           style_leader,
                           "\n")

    style_element <- html("style",style_content,linebreak=TRUE)
    res <- html_group(style_element,res)
    res <- as.character(res)
    return(res)
}


