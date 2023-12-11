mat_format_stdstyle <- c(
  "padding-top"="3px",
  "padding-bottom"="3px",
  "padding-left"="0.5ex",
  "padding-right"="0.5ex",
  "margin-top"="0px",
  "margin-bottom"="0px",
  "border-style"="none",
  "border-width"="0px"
)

style_mat_global <- function(id=NULL,class=NULL,style=mat_format_stdstyle,margin="2ex auto") {

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

style_mat_rule <- function(id=NULL,class=NULL,top=FALSE,bottom=FALSE,rulewidth=1,rows=NULL){
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

style_mat_cols <- function(id=NULL,class=NULL,cols=NULL,style=""){
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

style_mat_header <- function(id=NULL,class=NULL,style=""){
    class_or_id <- class_id_selector(id=id,class=class)
    selector_tmpl <- "table<<class-or-id>> td:nth-child(n of .header)"
    selector <- fillin(selector_tmpl,c("class-or-id"=class_or_id,col=col))
    paste0("\n",selector,"{",style,"}")
}

mat_html_env <- new.env()
mat_html_env$counter <- 1

format_html.matrix <- function(x,
                               toprule=2,midrule=1,bottomrule=2,
                               split.dec=TRUE,
                               formatC=FALSE,
                               digits=getOption("digits"),
                               format="f",
                               style=mat_format_stdstyle,
                               margin="2ex auto",
                               ...){

    
    colsep <- ""
    rowsep <- "\n"
    
    n <- nrow(x)
    m <- ncol(x)
    dim.x <- dim(x)

    dot_cols <- integer(0)
    col_sum <- 0

    colspan <- integer(m)
  
    if(is.integer(x)){
        tmp <- formatC(x,format="d")
        body <- html_td(tmp,vectorize=TRUE)
        colspan <- 1L
    }
    else if(is.numeric(x)){
        if(formatC)
            tmp <- formatC(x,digits=digits,format=format)
        else
            tmp <- format(x)
        if(split.dec){
            ncol.tmp <- ncol(tmp)
            tmp <- spltDec(tmp)
            body <- html_td_spltDec(tmp)
            colspan <- 3L
            dot_cols <- (1:ncol.tmp-1)*3 + 2
        }
        else{
            body <- html_td(tmp,vectorize=TRUE)
            colspan <- 1L
        }
    }
    else {
        tmp <- as.character(x)
        body <- html_td(tmp,vectorize=TRUE)
        colspan <- 1L
    }
    dim(body) <- dim.x
    
    if(length(rownames(x))){
        tmp <- rownames(x)
        ldr <- html_td(tmp,vectorize=TRUE)
        body <- cbind(ldr,body)
        if(length(dot_cols)>0)
            dot_cols <- dot_cols + 1
    }

    body <- apply(body,1,html_tr)

    if(length(colnames(x))){
        
        hdr <- colnames(x)
        if(length(rownames(x))){
            hdr <- c("",hdr)
            colspan <- c(1L,rep(colspan,m))
        }
        else
            colspan <- rep(colspan,m)
        hdr <- html_td(hdr,vectorize=TRUE,class="header")
        hdr[] <- mapply(setAttribs,hdr,colspan=colspan,SIMPLIFY=FALSE)
        hdr <- html_tr(hdr)
        
        res <- c(list(hdr),body)
    }
    else
        res <- body

    mat_id <- paste0("matrix-",mat_html_env$counter)
    res <- html_table(res,id=mat_id)

    mat_html_env$counter <- mat_html_env$counter + 1
    
    style_global <- style_mat_global(id=mat_id,style=style,margin=margin)
    style_toprule <- style_mat_rule(id=mat_id,rulewidth=toprule,top=TRUE,
                                       rows=1)
    if(length(colnames(x))){
        style_bottomrule <- style_mat_rule(id=mat_id,rulewidth=bottomrule,bottom=TRUE,
                                          rows=nrow(x)+1)
        style_midrule <- style_mat_rule(id=mat_id,rulewidth=midrule,bottom=TRUE,
                                          rows=1)
    }
    else{
        style_bottomrule <- style_mat_rule(id=mat_id,rulewidth=bottomrule,bottom=TRUE,
                                          rows=nrow(x))
        style_midrule <- NULL
    }


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
        style_dots <- style_mat_cols(id=mat_id,cols=dot_cols+1,style=dot_style)
        style_before_dots <- style_mat_cols(id=mat_id,cols=dot_cols,style=before_dot_style)
        style_behind_dots <- style_mat_cols(id=mat_id,cols=dot_cols+2,style=behind_dot_style)
        style_content <- paste(style_content,
                               style_dots,
                               style_before_dots,
                               style_behind_dots,sep="\n")
    }
    if(length(colnames(x))){
        header_style <- "
         padding-left: 0.5ex;
         padding-right: 0.5ex;
         text-align: center; 
         /*background-color: gray;*/
        "
        style_header <- style_mat_header(id=mat_id,style=header_style)
        style_content <- paste(style_content,
                               style_header,
                               "\n")
    }
    if(length(rownames(x))){
        leader_style <- "
         padding-left: 0.5ex;
         padding-right: 0px;
         text-align: right; 
         /*background-color: red;*/
        "
        style_leader <- style_mat_cols(id=mat_id,cols=1,style=leader_style)
        style_content <- paste(style_content,
                               style_leader,
                               "\n")
    }

    style_element <- html("style",style_content,linebreak=TRUE)
    res <- html_group(style_element,res)
    res <- as.character(res)
    return(res)
}


