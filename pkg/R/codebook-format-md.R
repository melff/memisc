format_md.codebook <- function(x, ...) {
  out <- mapply(format_md, x = x@.Data, name = names(x),
                   MoreArgs = list(...))
  unlist(out)
}

format_md.codebookEntry <- function(x, name = "", add_rules = TRUE, ...) {
  blank_line <- "\n"
  width <- getOption("width")
  hrule <- if (add_rules) paste(rep("-",width),collapse="")
                     else NULL


  annot <- x@annotation
  description <- annot["description"]
  wording <- annot["wording"]
  if(length(annot)) annot <- annot[names(annot) %nin% c("description","wording")]

  title <- strwrap(if(length(description) && !is.na(description))
                  paste0(name[1]," --- ",sQuote(description))
                  else name[1] ,width=width,prefix="   ")
  
  wording <- if(length(wording) && !is.na(wording))
                strwrap(dQuote(wording),width=width,prefix="   ")
             else NULL

  spec <- paste("  ",names(x@spec),x@spec,"   ")
  tab <- unclass(x@stats$tab)
  descr <- unclass(x@stats$descr)
  
  if(length(tab)){
      
    tab.title <- attr(tab,"title")
    tab.d <- dim(tab)
    tab.dn <- dimnames(tab)
    tab_md <- apply(tab,3,format_cb_table_md)

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

    tab_val <- format_col_md(tab_val,
                             align="r",
                             colname="Values")
    tab_m <- format_col_md(tab_m,
                           align="r",
                           colname="")
    tab_lab <- format_col_md(tab_lab,
                             align="l",
                             colname="Labels")
    tab_ldr <- paste0(tab_val,"|",tab_m,"|",tab_lab)
    tab <- paste0("   |",tab_ldr,tab_md,"|")
  }
  if(!is.matrix(descr)) descr <- NULL
  if(length(descr)){
    descr.rn <- format(paste(rownames(descr),":",sep=""),justify="left")
    if(is.numeric(descr[]))  
       descr[] <- formatC(descr[],format="f",digits=3)
    descr[] <- gsub("NA","",descr[])
    if(!length(ncol(descr))) browser()
    if(ncol(descr) > 1){
      descr.rn <- c("","",descr.rn)
      descr <- rbind(colnames(descr),"",descr)
    }
    descr <- cbind(descr.rn,descr)
    descr <- apply(descr,2,format,justify="right")
    descr <- paste0("   ",apply(descr,1,paste,collapse=" "),"   ")
  }
  if(length(tab) && length(descr)){
    statstab <- format(c(tab,"",descr),justify="left")
  }
  else if(length(tab)){
    statstab <- tab
  }
  else if(length(descr)){
    statstab <- descr
  }
  else
    statstab <- NULL
  
  annot.out <- character()
  if(length(annot)){
    for(i in seq_len(length(annot))){
      annot.i <- annot[i]
      nm.i <- trimws(names(annot.i))
      annot.i <- strwrap(annot.i,width=getOption("width")-8-4)
      annot.i <- c(paste("      ",annot.i),"")
      if(nzchar(nm.i)){
        annot.i <- c(
          paste("   ",nm.i,":",sep=""),
          annot.i
          )
      }
      annot.out <- c(annot.out,annot.i)
    }
  }
  c(
    hrule,
    "",
    title,
    if(length(wording)) c(
        "",
        wording
        ),
    "",
    hrule,
    "",
    spec,
    "",
    statstab,
    "",
    if(length(annot.out)) annot.out
    )

  
}

format_cb_table_md <- function(tab){
    cn <- colnames(tab)
    if(ncol(tab)>2){
        if(all(trunc(tab[,1])==tab[,1])){
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="d"),
                formatC(tab[,2],format="f",digits=1),
                formatC(tab[,3],format="f",digits=1)
            )
        } else {
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="f",digits=1),
                formatC(tab[,2],format="f",digits=1),
                formatC(tab[,3],format="f",digits=1)
            )
        }
    }
    else {
        if(all(trunc(tab[,1])==tab[,1])){
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="d"),
                formatC(tab[,2],format="f",digits=1)
            )
        } else {
            tab <- cbind(
                formatC(tab[,1,drop=FALSE],format="f",digits=1),
                formatC(tab[,2],format="f",digits=1)
            )
        }
        
    }
    tab[tab=="NA"] <- ""
    tab <- rbind(" "=cn,"",tab)
    tab <- format(tab,justify="right")
    tab[] <- paste0(" ",tab," ")
    specline <- sapply(nchar(tab[2,]),md_tabspec,align="r")
    tab[2,] <- specline
    tab <- apply(tab,1,paste,collapse="|")
    tab <- paste0("|",tab)
    tab
}

# This assumes that the contents are already formatted!
format_col_md <- function(x,align,colname){
    if(align=="r")
        x <- format(x,justify="right")
    else if(align=="l")
        x <- format(x,justify="left")
    x <- c(colname,x)
    x <- paste0(" ",format(x)," ")
    n <- nchar(x[1])
    spec <- md_tabspec(n,align=align)
    c(x[1],spec,x[-1])
}

md_tabspec <- function(n,align="l"){
    spec <- rep("-",n)
    if(align=="l")
        spec[1] <- ":"
    else if(align=="r")
        spec[n] <- ":"
    paste(spec,collapse="")
}
