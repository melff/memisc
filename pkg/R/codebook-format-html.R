
trimws_drpz <- function(x){
  x <- trimws(x)
  x[nzchar(x)]
}

format_html.codebookEntry <- function(x,name="",
                                      toprule=2,
                                      midrule=1,
                                      padding=3,
                                      var_tag="code",
                                      varid_prefix="",
                                      title_tag="p",
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
  if(length(padding))
    padding <- c("padding-left"=paste0(padding,"ex"))
  
  annot <- x@annotation
  description <- annot["description"]
  wording <- annot["wording"]
  if(length(annot)) annot <- annot[names(annot) %nin% c("description","wording")]
  
  title_html <- mk_elem(name[1],type=var_tag,class="cbe-name")
  if(length(description) && !is.na(description))
    title_html <- paste(title_html," &mdash; ",mk_span(sQuote(description),class="cbe-description"))
  
  title_html <- mk_scope(title_html,type=title_tag,class="cbe-title",id=paste0(varid_prefix,name[1]))
  
  wording_html <- if(length(wording) && !is.na(wording)) 
    mk_p(dQuote(wording), class="cbe-wording") 
  else NULL
  
  
  spec_html <- cbind(mk_td(names(x@spec),style=proc_style(align.left)),
                     mk_td(x@spec,style=proc_style(align.left)))
  spec_html[] <- gsub("--","&ndash;",spec_html)
  spec_html <- apply(spec_html,1,paste0,collapse="")
  spec_html <- mk_tr(spec_html)
  spec_html <- mk_p(mk_table(spec_html,class="cbe-spec"))
  
  tab <- x@stats$tab
  descr <- x@stats$descr
  
  if(length(tab)){
    
    tab_html <- cbind(
      formatC(tab[,1],format="d"),
      formatC(tab[,2],format="f",digits=1),
      formatC(tab[,3],format="f",digits=1)
    )
    tab_html[tab_html=="NA"] <- ""
    tab_html <- cbind(tab_html[,1],t(apply(tab_html[,-1,drop=FALSE],1,spltDec)))
    
    tab_html[,1] <- mk_td(tab_html[,1],style=proc_style(c(align.right,lrpad)))
    tab_html[,-1] <- mk_td_spltDec(tab_html[,-1,drop=FALSE])
    
    tab_lab <- trimws(rownames(tab))
    tab_lab <- strsplit(tab_lab," ")
    tab_lab <- lapply(tab_lab,trimws_drpz)
    
    tab_has_lab <- sapply(tab_lab,length) > 1
    tab_has_m <- sapply(sapply(tab_lab,"[",2) == "M",isTRUE)
    
    tab_val <- ifelse(tab_has_lab,sapply(tab_lab,"[",1),"")
    tab_lab <- ifelse(tab_has_lab,sapply(tab_lab,"[",-1,drop=FALSE),tab_lab)
    
    tab_m <- ifelse(tab_has_m,"M","")
    tab_lab <- ifelse(tab_has_m,sapply(tab_lab,"[",-1,drop=FALSE),tab_lab)
    
    tab_lab <- sapply(tab_lab,paste,collapse=" ")
    
    tab_lab_html <- cbind(mk_td(tab_val,style=proc_style(c(align.right,lrpad))),
                          mk_td(tab_m,style=proc_style(c(align.center,lrpad))),
                          mk_td(tab_lab,style=proc_style(c(align.left,lrpad))))
    
    tab_html <- cbind(tab_lab_html,tab_html)
    tab_html <- apply(tab_html,1,paste0,collapse="")
    tab_html <- mk_tr(tab_html)
    
    tabh_html <- c(mk_td("Values and labels",colspan=3,style=proc_style(align.center)),
                   mk_td("N",style=proc_style(align.center)),
                   mk_td("Percent",colspan=6,style=proc_style(align.center)))
    tabh_html <- mk_tr(paste0(tabh_html,collapse=""))
    
    tab_html <- mk_table(c(tabh_html,tab_html),class="cbe-table")
    tab_html <- mk_p(tab_html)
  }
  else tab_html <- NULL
  
  if(length(descr)){
    
    descr_html <- cbind(
      trimws(paste(names(descr),": ",sep="")),
      formatC(descr,format="f",digits=3)
    )
    
    descr_html <- cbind(descr_html[,1],t(sapply(descr_html[,-1,drop=FALSE],spltDec)))
    
    descr_html[,1] <- mk_td(descr_html[,1],style=proc_style(c(align.right,lrpad)))
    descr_html[,-1] <- mk_td_spltDec(descr_html[,-1,drop=FALSE])
    descr_html <- mk_tr(apply(descr_html,1,paste0,collapse=""))
    descr_html <- mk_table(descr_html,class="cbe-table")
    descr_html <- mk_p(descr_html)
  } else descr_html <- NULL
  
  if(length(annot)){
    annot_html <- cbind(mk_elem(paste0(names(annot),":"),type="dt"),
                        mk_elem(annot,type="dd"))
    annot_html <- mk_scope(c(t(annot_html)),type="dl",class="cbe-annotations")
    annot_html <- mk_p(annot_html)
  } else annot_html <- NULL
  
  mk_div(c(
    mk_div(c(title_html,wording_html),class="cbe-header",style=proc_style(c(toprule,midrule,padding))),
    mk_div(c(spec_html,tab_html,descr_html,annot_html),class="cbe-body",style=proc_style(padding))
  ),class="codebook-entry")
  
}

format_html.codebook <- function(x,toprule=2,
                                 midrule=1,
                                 padding=3,
                                 var_tag="code",
                                 varid_prefix="",
                                 title_tag="p",
                                 ...)
{
  out <- mapply(format_html,x=x,name=names(x),
                MoreArgs = list(
                    toprule=toprule,midrule=midrule,
                    padding=padding,var_tag=var_tag,
                    varid_prefix=varid_prefix,
                    title_tag=title_tag
                  ))
  mk_div(unlist(out),class="codebook")
}