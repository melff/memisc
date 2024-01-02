ftab_mat_html_env <- new.env()
ftab_mat_html_env$counter <- 1

style_ftab_matrix_group_rule <- function(id=NULL,class=NULL,rulewidth=1){
    class_or_id <- class_id_selector(id=id,class=class)
    rulewidth <- paste0(rulewidth,"px")
    selector_tmpl <- "table<<class-or-id>> td:nth-child(n of .header)"
    selector <- fillin(selector_tmpl,c("class-or-id"=class_or_id))
    style_tmpl <- paste("<<selector>> {",
                        "     border-bottom: <<rulewidth>> solid;",
                        "}",sep="\n")
    fillin(style_tmpl,c(selector=selector,rulewidth=rulewidth))
}

format_html.ftable_matrix <- function(x,
                                      show.titles=TRUE,
                                      digits=0,
                                      format="f",
                                      toprule=2,midrule=1,bottomrule=2,
                                      split.dec=TRUE,
                                      style=ftable_format_stdstyle,
                                      margin="2ex auto",
                                      varontop,
                                      varinfront,
                                      grouprules=1,
                                      multi_digits=NULL,
                                      ...){
    
    row.vars <- attr(x,"row.vars")
    col.vars <- attr(x,"col.vars")

    nms.cv <- names(col.vars)
    n.col.vars <- length(col.vars)

    N <- nrow(x)
    M <- ncol(x)
    
    n <- sapply(x[,1],nrow)
    m <- sapply(x[1,],ncol)
    
    l.cv <- sapply(col.vars,length)
    max.l.cv <- max(l.cv)
    
    l.rv <- sapply(row.vars,length)
    max.l.rv <- max(l.rv)

    if(missing(varontop)) varontop <- (max.l.cv <= 1)
    if(missing(varinfront)) varinfront <- (max.l.rv <= 1)
    
    if(!missing(multi_digits)){
        if(is.list(multi_digits)){
            digits <- multi_digits
        }
        else {
            digits <- rep(list(multi_digits),M)
        }
    }
    else {
        d <- digits
        digits <- integer(M)
        digits[] <- d
    }
    
    fo <- format
    format <- integer(M)
    format[] <- fo
    
    
    headers <- mapply(htfm_mkHeader,col.vars,m,
                      MoreArgs=list(max.l.cv,varontop,split.dec),SIMPLIFY=FALSE)
    leaders <- mapply(htfm_mkLeader,row.vars,n,
                      MoreArgs=list(max.l.rv,varinfront),SIMPLIFY=FALSE)

    body <- array(list(),dim=dim(x))
    
    mm <- length(headers[[1]])
    nn <- ncol(leaders[[1]])
    
    lheaders <- matrix("",nrow=mm,ncol=nn)

    dot_cols <- integer(0)
    col_sums <- 0
    
    for(i in 1:N){
        rv.i <- row.vars[[i]]

        if(l.rv[i] > 1 || (!varinfront && nzchar(names(rv.i)))){
            
            n.rv.i <- names(rv.i)
            if(l.rv[i] > 1 && varinfront)
                n.rv.i[1] <- ""

            if(i == 1){
                lheaders[mm,] <- n.rv.i
            }
            else {
                tmp.ldr <- leaders[[i]]
                tmp.ldr <- rbind(html_td(n.rv.i,vectorize=TRUE),tmp.ldr)
                leaders[[i]] <- tmp.ldr
            }
        }
        if(i < N) {
            tmp.ldr <- leaders[[i]]
            leaders[[i]] <- tmp.ldr
        }
        
        for(j in 1:M){
            cv.j <- col.vars[[j]]
            tmp.bdy <- htfm_mkBody(x[[i,j]],format[[j]],digits[[j]],split.dec)
            if(i == 1 && split.dec) {
                nm.cv.j <- names(cv.j)
                nz.nm.cv.j <- length(nm.cv.j) && all(nzchar(nm.cv.j))
                m.j <- ncol(tmp.bdy)
                if((!varontop || max.l.cv > 1) && nz.nm.cv.j) {
                    col_sums <- col_sums + 1
                    # message(sprintf("Incremented col_sums j=%d varontop=%d max.l.cv=%d col_sums=%d",j,varontop,max.l.cv,col_sums))
                    # print(dot_cols)
                }
                dot_cols <- c(dot_cols, col_sums + (1:m.j-1)*3 + 2)
                col_sums <- col_sums + 3*m.j
            }
            if(i > 1 && (l.rv[i] > 1 || (!varinfront && nzchar(names(rv.i))))){

                if(split.dec){
                    tmp. <- spltDec(rep("",ncol(tmp.bdy)))
                    tmp.bdy <- rbind(html_td_spltDec(tmp.),tmp.bdy)
                }        
                else
                    tmp.bdy <- rbind(html_td(rep("",ncol(tmp.bdy)),vectorize=TRUE),tmp.bdy)
            }
            if(l.cv[j] > 1 || (!varontop && nzchar(names(cv.j)))){
                
                tmp.bdy <- cbind(html_td(rep("",nrow(tmp.bdy)),vectorize=TRUE),tmp.bdy)
            }
            if(i < N){
                nnn <- nrow(tmp.bdy)
            }
            body[[i,j]] <- tmp.bdy
        }
    }

    lheaders <- array(html_td(lheaders,vectorize=TRUE),
                      dim=dim(lheaders))

    leaders <- rbind(lheaders,do.call(rbind,leaders))
    
    res <- list()
    for(j in 1:M){
        res.j <- body[,j]
        res.j <- do.call(rbind,res.j)
        res.j <- apply(res.j,1,as.html_group)
        res[[j]] <- as.html_group(c(headers[[j]],res.j))
    }
    res <- do.call(cbind,res)
    res <- cbind(leaders,res)

    nh <- nrow(lheaders)
    nl <- ncol(leaders)
    ntot <- nrow(res)
    
    res <- apply(res,1,as.html_group)
    
    res <- html_tr(res,vectorize=TRUE)
    
    ftab_id <- paste0("ftab-matrix-",ftab_mat_html_env$counter)
    res <- html_table(res,class="ftable",id=ftab_id)

    ftab_mat_html_env$counter <- ftab_mat_html_env$counter + 1    

    style_global <- style_ftab_global(id=ftab_id,style=style,margin=margin)
    style_toprule <- style_ftab_rule(id=ftab_id,rulewidth=toprule,top=TRUE,
                                       rows=1)
    style_bottomrule <- style_ftab_rule(id=ftab_id,rulewidth=bottomrule,bottom=TRUE,
                                          rows=ntot)
    nrows <- sapply(x,nrow)
    dim(nrows) <- dim(x)
    nrows <- nrows[,1]

    if(length(nrows) > 1){
        if(varinfront){
            midrule_rows <- cumsum(c(nh,nrows))
        }
        else {
            nrows <- nrows[-1]
            l.nr <- length(nrows)
            if(grouprules > 1)
                midrule_rows <- c(nh,nrows + (rep(1:l.nr,each=2) - c(1,0)) + nh)
            else
                midrule_rows <- c(nh,nrows + 1:l.nr - 1 + nh)
        }
    }
    else midrule_rows <- nh

    style_midrule <- style_ftab_rule(id=ftab_id,rulewidth=midrule,bottom=TRUE,
                                          rows=midrule_rows)
    style_grouprule <- style_ftab_matrix_group_rule(id=ftab_id,rulewidth=midrule)

    style_content <- paste(
        style_global,
        style_toprule,
        style_midrule,
        style_bottomrule,
        style_grouprule,
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
        dot_cols <- dot_cols + nl
        style_dots <- style_df_cols(id=ftab_id,cols=dot_cols,style=dot_style)
        style_before_dots <- style_df_cols(id=ftab_id,cols=dot_cols-1,style=before_dot_style)
        style_behind_dots <- style_df_cols(id=ftab_id,cols=dot_cols+1,style=behind_dot_style)
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


htfm_mkHeader <- function(col.vars,m,max.l.cv,varontop,split.dec){
 
  nms.cv <- names(col.vars)
  n.col.vars <- length(col.vars)
  
  header <- vector("list",length=n.col.vars)
  mm <- 1
  for(i in rev(1:n.col.vars)){
    
    cv <- col.vars[[i]]
    ncv <- length(cv)
    if(split.dec)
      colspan <- mm*3
    else
      colspan <- mm
    mm <- mm*ncv
    cv <- rep(cv,m%/%mm)
    
    header.i <- html_td(cv,vectorize=TRUE,colspan=colspan,class="header") 
    
    if(n.col.vars > 1 || !varontop){
      
      if(i == 1 && varontop || !length(nms.cv)){
        htmp <- html_td("")
      }
      else {
        if(!nzchar(nms.cv[i])){
          htmp <- NULL
        }
        else
          htmp <- html_td(paste0(nms.cv[i],":"),class="header")
      }    
      if(length(htmp))
        header.i <- c(htmp,header.i)
    }
    header[[i]] <- header.i
  }
  
  if(varontop){

    if(split.dec)
      colspan <- mm*3
    else
      colspan <- mm
    
    if(n.col.vars > 1){
      htmp1 <- html_td("")
      htmp2 <- html_td(nms.cv[1],colspan=colspan,class="header")
      htmp <- c(htmp1,htmp2)
    }
    else if(nzchar(nms.cv[1])){
      htmp <- html_td(names(col.vars)[1],colspan=colspan,class="header")
    }
    else {
      htmp <- html_td("",colspan=colspan)
    }
    header <- c(list(htmp),header)
  }
  
  tmp.header <- header

  if(split.dec)
    colspan <- m*3
  else
    colspan <- m
  if(n.col.vars > 1 || !varontop)
    colspan <- colspan + 1
  
  l.header <- max.l.cv + varontop

  header <- rep(list(html_td("",colspan=colspan)),l.header)
  
  i <- seq(to=l.header,length=length(tmp.header))
  header[i] <- tmp.header
  header
}

htfm_mkLeader <- function(row.vars,n,max.l.rv,varinfront) {
  
  align.left <- c("text-align"="left")  
  n.row.vars <- length(row.vars)
  nms.row.vars <- names(row.vars)
  leader <- matrix("",nrow=n,ncol=n.row.vars)
  
  for(ii in 1:n.row.vars){
    
    rv <- row.vars[[ii]]
    rep.rv <- if(ii==1)1 else nn.rv
    rv <- rep(rv,rep.rv)
    nn.rv <- length(rv)
    rv.n <- n/nn.rv
    pos <- (1:nn.rv)*rv.n
    pos <- pos - rv.n + 1
    leader[pos,ii] <- rv
  }
  if(varinfront){
    lleader <- character(n)
    if(nzchar(nms.row.vars[1]))
      lleader[1] <- nms.row.vars[1]
    else if(n==1){
      lleader[1] <- leader[1,1]
      leader[1,1] <- ""
    }
      
    leader <- cbind(lleader,leader)
    if(n.row.vars > 1){
      hleader <- character(n.row.vars+1)
      ii <- 2:n.row.vars
      hleader[ii] <- nms.row.vars[ii]
      leader <- rbind(hleader,leader)
    }
  }
  
  tmp.leader <- leader
  leader <- matrix("",nrow=nrow(leader),ncol=max.l.rv+as.integer(varinfront))
  ii <- seq(to=ncol(leader),length=ncol(tmp.leader))
  leader[,ii] <- tmp.leader
  
  leader <- array(html_td(leader,vectorize=TRUE),dim=dim(leader))
  leader
}

htfm_mkBody <- function(x,format,digits,split.dec){
  
  n <- nrow(x)
  m <- ncol(x)
  
  d <- digits
  digits <- integer(m)
  digits[] <- d
  
  fo <- format
  format <- integer(m)
  format[] <- fo
  
  body <- array(list(),dim=c(n,m))
  
  for(i in 1:m){
    tmp <- formatC(x[,i],digits=digits[i],format=format[i])
    if(getOption("html.use.ampersand",FALSE))
      tmp <- gsub("-","&minus;",tmp,fixed=TRUE)
    else
      tmp <- gsub("-","\u2212",tmp,fixed=TRUE)
    if(!split.dec)
      body[,i] <- html_td(tmp,vectorize=TRUE)
    else {
      tmp <- spltDec(tmp)
      tmp <- html_td_spltDec(tmp)
      body[,i] <- tmp
    }
  }
  body
}

