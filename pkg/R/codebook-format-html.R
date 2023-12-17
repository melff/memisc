
trimws_drpz <- function(x){
  x <- trimws(x)
  x[nzchar(x)]
}

format_cb_table_ht <- function(tab){
    cn <- colnames(tab)
    if(ncol(tab)>2){
        tab <- cbind(
            formatC(tab[,1,drop=FALSE],format="d"),
            formatC(tab[,2],format="f",digits=1),
            formatC(tab[,3],format="f",digits=1)
        )
    }
    else {
        tab <- cbind(
            formatC(tab[,1,drop=FALSE],format="d"),
            formatC(tab[,2],format="f",digits=1)
        )
    }
    tab[tab=="NA"] <- ""
    tab <- rbind(" "=cn,"",tab)
    tab
}

format_html.codebookEntry <- function(x,name="",
                                      var_tag="code",
                                      varid_prefix="",
                                      title_tag="p",
                                      ...
){
  
  annot <- x@annotation
  description <- annot["description"]
  wording <- annot["wording"]
  if(length(annot)) annot <- annot[names(annot) %nin% c("description","wording")]
  
  title_html <- html(var_tag,.content=name[1],class="cbe-name")
  if(length(description) && !is.na(description))
    title_html <- html_group(title_html," &mdash; ",html("span",.content=sQuote(description),class="cbe-description"))
  
  title_html <- html(title_tag,.content=title_html,class="cbe-title",id=paste0(varid_prefix,name[1]))
  title_html <- html_group(html_p(""),title_html)
  
  wording_html <- if(length(wording) && !is.na(wording)) 
    html_p(dQuote(wording), class="cbe-wording") 
  else NULL
  x.spec <- gsub("--","&ndash;",x@spec)
  spec_html <- cbind(html_td(names(x.spec),vectorize=TRUE),
                     html_td(x.spec,vectorize=TRUE))
  spec_html <- apply(spec_html,1,html_tr)
  spec_html <- as.html_group(spec_html)
  
  spec_html <- html_table(spec_html,class="cbe-spec")
  spec_html <- html_group(html_p(""),spec_html)
  
  tab <- x@stats$tab
  descr <- x@stats$descr
  
  if(length(tab)){
    counts_html <- formatC(tab[,1,1],format="d")
    counts_html <- html_td(counts_html,vectorize=TRUE)

    perc_html <- formatC(tab[,-1,1,drop=FALSE],
                         format="f",digits=1)
    perc_html[perc_html=="NA"] <- ""
    perc_html <- spltDec(perc_html)
    perc_html <- html_td_spltDec(perc_html)

    tab_html <- c(
      counts_html,
      perc_html
    )

    if(dim(tab)[3] > 1){
      counts_html <- formatC(tab[,1,1],
                             format="f",digits=1)
      counts_html <- spltDec(counts_html)
      counts_html <- html_td_spltDec(counts_html)

      perc_html <- formatC(tab[,-1,2,drop=FALSE],
                            format="f",digits=1)
      perc_html[perc_html=="NA"] <- ""
      perc_html <- spltDec(perc_html)
      perc_html <- html_td_spltDec(perc_html)
      wtab_html <- c(
        counts_html,
        perc_html
      )
      tab_html <- c(tab_html,wtab_html)
    }  
    tab_html <- matrix(tab_html, nrow = nrow(tab))

    tab_rown <- trimws(rownames(tab))
    
    tab_lab <- strsplit(tab_rown," ")

    tab_lab <- lapply(tab_lab,trimws_drpz)
    
    tab_has_lab <- sapply(tab_lab,length) > 1
    tab_has_m <- sapply(sapply(tab_lab,"[",2) == "M",isTRUE)
    
    tab_val <- ifelse(tab_has_lab,sapply(tab_lab,"[",1),"")
    tab_lab <- ifelse(tab_has_lab,lapply(tab_lab,"[",-1,drop=FALSE),tab_lab)
    
    tab_m <- ifelse(tab_has_m,"M","")
    tab_lab <- ifelse(tab_has_m,lapply(tab_lab,"[",-1,drop=FALSE),tab_lab)
    
    tab_lab <- sapply(tab_lab,paste,collapse=" ")
    
    tab_lab_html <- cbind(html_td(tab_val,vectorize=TRUE),
                          html_td(tab_m,vectorize=TRUE),
                          html_td(tab_lab,vectorize=TRUE))
    
    tab_html <- cbind(tab_lab_html,tab_html)
    tab_html <- as.html_group(apply(tab_html,1,html_tr))

    if(dim(tab)[3] == 1){
      if(dim(tab)[2] == 3){
        tabh_html <- html_group(html_td("Values and labels",colspan=3,class="header"),
                                html_td("N",class="header"),
                                html_td("Valid",colspan=3,class="header"),
                                html_td("Total",colspan=3,class="header"))
      }
      else{
        tabh_html <- html_group(html_td("Values and labels",colspan=3,class="header"),
                                html_td("N",class="header"),
                                html_td("Percent",colspan=3,class="header"))
      }
      tabh_html <- html_tr(tabh_html)
    } else {
      if(dim(tab)[2] == 3){
        tabh_html2 <- html_group(html_td("Values and labels",colspan=3,class="header"),
                                html_td("N",class="header"),
                                html_td("Valid",colspan=3,class="header"),
                                html_td("Total",colspan=3,class="header"),
                                html_td("N",colspan=3,class="header"),
                                html_td("Valid",colspan=3,class="header"),
                                html_td("Total",colspan=3,class="header"))
        tabh_html1 <- html_group(html_td("",colspan=3),
                               html_td("Unweighted",colspan=7,class="header"),
                               html_td("Weighted",colspan=9,class="header"))
      }
      else{
        tabh_html2 <- html_group(html_td("Values and labels",colspan=3,class="header"),
                                html_td("N",class="header"),
                                html_td("Percent",colspan=3,class="header"),
                                html_td("N",colspan=3,class="header"),
                                html_td("Percent",colspan=3,class="header"))
        tabh_html1 <- html_group(html_td("",colspan=3),
                               html_td("Unweighted",colspan=4,class="header"),
                               html_td("Weighted",colspan=6,class="header"))
      }
      
      tabh_html1 <- html_tr(tabh_html1)
      tabh_html2 <- html_tr(tabh_html2)
      tabh_html <- c(tabh_html1,tabh_html2)
    }
    
    tab_html <- html_table(html_group(tabh_html,tab_html),class="cbe-table cbe-vl-table")
    tab_html <- html_group(html_p(""),tab_html)
  }
  else tab_html <- NULL
  
  if(length(descr)){
    descr_ldr_html <- trimws(paste(rownames(descr),": ",sep=""))
    descr_ldr_html <- html_td(descr_ldr_html,vectorize=TRUE)
    descr_html <- formatC(descr,format="f",digits=3)
    
    descr_html <- spltDec(descr_html)
    descr_html <- html_td_spltDec(descr_html)

    dim(descr_html) <- dim(descr)

    descr_html <- cbind(descr_ldr_html,descr_html)
    descr_html <- apply(descr_html,1,html_tr)

    if(ncol(descr)>1){
      descrh_html <- html_group(html_td(""),
                                html_td("Unweighted",colspan=3,class="header"),
                                html_td("&nbsp;Weighted",colspan=3,class="header"))
      descr_html <- c(list(html_tr(descrh_html)),descr_html)
    }
    
    descr_html <- html_table(descr_html,class="cbe-table cbe-d-table")
    descr_html <- html_group(html_p(""),descr_html)
  } else descr_html <- NULL
  
  if(length(annot)){
    annot_html <- cbind(html_dt(paste0(names(annot),":"),vectorize=TRUE),
                        html_dd(annot,vectorize=TRUE))
    annot_html <- as.html_group(t(annot_html))
    annot_html <- html("dl",.content=annot_html,class="cbe-annotations")
    annot_html <- html_group(html_p(""),annot_html)
  } else annot_html <- NULL
  
  header_html <- html_div(html_group(title_html,wording_html),class="cbe-header")
  body_html <- html_div(html_group(spec_html,tab_html,descr_html,annot_html,html_p("")),class="cbe-body")

  cbe_html <- html_div(
      html_group(header_html,body_html),class="codebook-entry")
  cbe_html <- as.character(cbe_html)
  return(cbe_html)
}

codebook_format_stdstyle <- c(
    "padding-left"=".3em",
    "padding-right"=".3em"
)

format_html.codebook <- function(x,
                                 toprule=2,
                                 midrule=1,
                                 indent="3ex",
                                 style=codebook_format_stdstyle,
                                 var_tag="code",
                                 varid_prefix="",
                                 title_tag="p",
                                 ...)
{
  out <- mapply(format_html,x=x@.Data,name=names(x),
                MoreArgs = list(
                    var_tag=var_tag,
                    varid_prefix=varid_prefix,
                    title_tag=title_tag
                  ))
  res <- html_div(unlist(out),class="codebook")

  cb_format_tmpl <- "
    .cbe-body table {
         margin: unset;
    }
    .cbe-body table,
    .cbe-body table tr,
    .cbe-body table td
    {
         border-style: none;
         padding: unset;
    }
    .cbe-header {
         border-top: solid 2px;
         border-bottom: solid 1px;
         padding-left: <<indent>>;
    }
    .cbe-body {
         padding-left: <<indent>>;
    }
    .cbe-table {
         border-collapse: collapse;
    }
    .cbe-table td:nth-child(n of .header) {
         text-align: center;
         padding-left: <<padding-left>>;
         padding-right: <<padding-right>>;
    }
    .cbe-vl-table td:nth-child(1) {
         text-align: right;
         padding-left: <<padding-left>>;
         padding-right: <<padding-right>>;
    }
    .cbe-vl-table td:nth-child(2) {
         text-align: center;
         padding-left: <<padding-left>>;
         padding-right: <<padding-right>>;
    }
    .cbe-vl-table td:nth-child(3) {
         text-align: left;
         padding-left: <<padding-left>>;
         padding-right: <<padding-right>>;
    }
    .cbe-vl-table td:nth-child(4) {
         text-align: right;
         padding-left: <<padding-left>>;
         padding-right: <<padding-right>>;
    }
    .cbe-vl-table td:nth-child(5),
    .cbe-vl-table td:nth-child(8),
    .cbe-vl-table td:nth-child(11),
    .cbe-vl-table td:nth-child(14),
    .cbe-vl-table td:nth-child(17),
    .cbe-d-table td:nth-child(2),
    .cbe-d-table td:nth-child(5){
         text-align: right;
         padding-left: <<padding-left>>;
         padding-right: 0px;
    }
    .cbe-vl-table td:nth-child(6),
    .cbe-vl-table td:nth-child(9),
    .cbe-vl-table td:nth-child(12),
    .cbe-vl-table td:nth-child(15),
    .cbe-vl-table td:nth-child(18),
    .cbe-d-table td:nth-child(3),
    .cbe-d-table td:nth-child(6) {
         text-align: center;
         padding-left: 0px;
         padding-right: 0px;
         width: 1px;
    }
    .cbe-vl-table td:nth-child(7),
    .cbe-vl-table td:nth-child(10),
    .cbe-vl-table td:nth-child(13),
    .cbe-vl-table td:nth-child(16),
    .cbe-vl-table td:nth-child(19),
    .cbe-d-table td:nth-child(4),
    .cbe-d-table td:nth-child(7) {
         text-align: left;
         padding-right: <<padding-left>>;
         padding-left: 0px;
    }
    .cbe-d-table td:nth-child(1){
         text-align: left;
         padding-left: <<padding-left>>;
         padding-right: <<padding-right>>;
    }
"
  toprule <- paste0(toprule,"px")
  midrule <- paste0(midrule,"px")
  style_content <- fillin(cb_format_tmpl,
                          c(codebook_format_stdstyle,
                            indent=indent,toprule=toprule,midrule=midrule))
  style_element <- html("style",style_content,linebreak=TRUE)
  res <- html_group(style_element,res)
  res <- as.character(res)
  return(res)
}
