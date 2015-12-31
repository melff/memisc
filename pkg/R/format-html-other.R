format_html.descriptions <- function(x,
                                     toprule=2,midrule=1,bottomrule=2,
                                     margin="2ex auto",
                                     ...){

  padleft <- c("padding-left"="0.3em")
  padright <- c("padding-right"="0.3em")
  toprule <- c("border-top"=paste0(midrule,"px solid"))
  bottomrule <- c("border-bottom"=paste0(midrule,"px solid"))
  midrule_above <- c("border-top"=paste0(midrule,"px solid"))
  midrule <- c("border-bottom"=paste0(midrule,"px solid"))
  align.right <- c("text-align"="right")  
  align.left <- c("text-align"="left")  
  align.center <- c("text-align"="center")


  tab <- cbind(
          html_td(names(x),style=css(align.left,padleft,padright),vectorize=TRUE),
          html_td(x,style=css(align.left,padright),vectorize=TRUE)
          )
  n <- nrow(tab)
  tab[n,] <- lapply(tab[n,],setStyle,bottomrule)
  
  header <- c(html_td("Variable",style=css(align.left,padleft,padright,toprule,midrule)),
              html_td("Description",style=css(align.center,padright,toprule,midrule)))
                    
  tab <- rbind(header,tab)
  ans <- apply(tab,1,html_tr)
  
  ans <- html_table(ans)
  
  table_style <- c("border-collapse"="collapse")
  if(length(margin))
    table_style <- c(table_style,margin=margin)
  style(ans) <- as.css(table_style)

  ans <- as.character(ans)
  return(ans)
}