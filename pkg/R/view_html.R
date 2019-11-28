html_grid <- function(x,title="",max_width=80,...){
    x <- as.matrix(x)
    n <- nchar(x)
    x[n>max_width] <- paste0(substr(x[n>max_width],start=1,stop=max_width),"...")
    tbody <- paste0("<td>",x,"</td>")
    dim(tbody) <- dim(x)
    tbody <- apply(tbody,1,paste0,collapse="")
    tbody <- paste0("<tr>",tbody,"</tr>")
    tbody <- c("<tbody>",tbody,"</tbody>")
    thead <- paste0("<th>",colnames(x),"</th>")
    thead <- paste0(thead,collapse="")
    thead <- paste0("<tr>",thead,"</tr>")
    thead <- c("<thead>",thead,"</thead>")
    table <- c(thead,tbody)
    table <- c("<table>",table,"</table>")
    style <- c("table { border: 0.5px solid #555; border-collapse:collapse; position: relative; }",
               "thead { background-color: #ddd; border: 0.5px solid #555;  }",
               "td { border: 0.5px solid #555; text-align: right; padding: 3px; white-space: nowrap; }",
               "th { border: 0.5px solid #555; text-align: center; padding: 3px;
                background-color: #ddd; position: sticky; top: 0;}",
               "body { margin: 0; }")
    style <- paste(style,collapse="\n")
    style <- c("<style>",style,"</style>")
    title <- paste0("<title>",title,"</title>")
    head <- c("<head>",title,style,"</head>")
    body <- c("<body>",table,"</body>")
    html <- c("<html>",head,body,"</html>")
    structure(html,class="raw_html")
}



view_html <- function(x,title=deparse(substitute(x)),output,...){
  
  ht <- html_grid(x,title,...)
  
  if(interactive()){
    # Test whether running under RStudio 
    isRStudio <- Sys.getenv("RSTUDIO") == "1"
    if(isRStudio)
      deflt.output <- "file-show"
    else
      deflt.output <- "browser"
  }
  else
    deflt.output <- "stdout"
  
  if(missing(output))
    output <- getOption("html_viewer",deflt.output)
  
  if(mode(output)=="character")
      output <- match.arg(output,c("stdout","browser","file-show"))
  else if(!is.function(output))
      stop("'output' should be either a character string of a function")
  
  if(is.function(output)){
    
    tf <- file.path(tempdir(),title)
    tf <- paste0(tf,".html")
    cat(ht,file=tf)
    
    output(tf)
  }
  else if(nzchar(Sys.getenv("JPY_PARENT_PID"))){
      ## Inside Jupyter 
      return(html_div(ht))
  }     
  else if(output=="stdout") cat(ht)
  else {
    
    tf <- file.path(tempdir(),title)
    tf <- paste0(tf,".html")
    cat(ht,file=tf)
    
    if(output=="file-show")
      file.show(tf,title=deparse(substitute(x)))
    else 
      browseURL(tf)
  }
  
}

