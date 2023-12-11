descr_format_stdstyle <- df_format_stdstyle

format_html.descriptions <- function(x,
                                     toprule=2,midrule=1,bottomrule=2,
                                     style=descr_format_stdstyle,
                                     margin="2ex auto",
                                     ...){

    tab <- cbind(
        html_td(names(x),vectorize=TRUE),
        html_td(x,vectorize=TRUE)
    )
    n <- nrow(tab)
    
    header <- c(html_td("Variable",class="header"),
                html_td("Description",class="header"))
    
    tab <- rbind(header,tab)
    res <- apply(tab,1,html_tr)
    
    res <- html_table(res,class="descriptions")

    style_global <- style_df_global(class="descriptions",style=style,margin=margin)

    style_toprule <- style_df_rule(class="descriptions",rulewidth=toprule,top=TRUE,
                                       rows=1)
    style_bottomrule <- style_df_rule(class="descriptions",rulewidth=bottomrule,bottom=TRUE,
                                          rows=n+1)
    style_midrule <- style_df_rule(class="descriptions",rulewidth=midrule,bottom=TRUE,
                                          rows=1)
    style_content <- paste(
        style_global,
        style_toprule,
        style_midrule,
        style_bottomrule,
        sep="\n"
    )
    style_element <- html("style",style_content,linebreak=TRUE)
    res <- html_group(style_element,res)
    res <- as.character(res)
    return(res)
}
