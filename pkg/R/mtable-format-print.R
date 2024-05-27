mkRule <- function(x,length) paste(rep_len(x,length),collapse="")

set_length <- function(x,n,fill=""){

    l <- length(x)
    nl <- min(n,l)

    y <- rep(fill,length=n)
    y[1:nl] <- x[1:nl]
    y
}

cols_folded <- function(x,n,sep){

    nc <- ncol(x)
    nr <- nrow(x)

    m <- nc/n
    dim(x) <- c(nr,n,m)
    y <- apply(x,c(1,3),paste,collapse=sep)
    y
}


mkGrpSpan <- function(hspan){

    hspan.out <- hspan
    l.hspan <- length(hspan)
    for(k in 2:l.hspan){
        hspan1 <- hspan[[k-1]]
        hspan2 <- hspan[[k]]
        hspan.out.k <- hspan2
        j1 <- 1
        s2 <- 0
        for(j2 in 1:length(hspan2)){
            s1 <- s2
            s2 <- s2 + hspan2[j2]
            c1 <- 0
            while(s1 < s2){
                s1 <- s1 + hspan1[j1]
                j1 <- j1 + 1
                c1 <- c1 + 1
            }
            if(s1 > s2) stop("misaligned spans")
            hspan.out.k[[j2]] <- c1
        }
        hspan.out[[k]] <- hspan.out.k
    }
    hspan.out
}

mtable_format_print <- function(x,
                                topsep="=",
                                bottomsep="=",
                                sectionsep="-",
                                interaction.sep = " x ",
                                center.at=getOption("OutDec"),
                                align.integers=c("dot","right","left"),
                                padding="  ",
                                ...)
    pf_mtable_format_print(preformat_mtable(x),
                                   topsep=topsep,
                                   bottomsep=bottomsep,
                                   sectionsep=sectionsep,
                                   interaction.sep =interaction.sep,
                                   center.at=center.at,
                                   align.integers=align.integers,
                                   padding=padding,
                                   ...)

pf_mtable_format_print <- function(x,
                                   topsep="=",
                                   bottomsep="=",
                                   sectionsep="-",
                                   interaction.sep = " x ",
                                   center.at=getOption("OutDec"),
                                   align.integers=c("dot","right","left"),
                                   padding="  ",
                                   show.parmtypes = nrow(x$parmtab) > 1,
                                   ...
                                   ){

    colsep <- " "
    rowsep <- "\n"

    pt <- x$parmtab
    sst <- x$summary.stats
    leaders <- x$leaders
    headers <- x$headers
    eq.headers <- x$eq.headers

    l.headers <- length(headers)
    
    res <- NULL
    ncols <- NULL

    has.eq.headers <- length(eq.headers) > 0
    
    for(j in 1:ncol(pt)){

        name.j <- colnames(pt)[j]
        pt.j <- pt[,j]
        l.pt.j <- length(pt.j)
        
        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        ncols <- c(ncols,ncol.j)

        skip.j <- logical(0)
        max.width <- 0

        maxncols.j <- max(unlist(lapply(pt.j,ncol)))
        
        nr.j <- numeric(l.pt.j)
        for(i in 1:l.pt.j){
            pt.ij <- pt.j[[i]]
            if(show.parmtypes){
                pt.ij <- rbind(" ",pt.ij)
            }
            nr.j[i] <- nrow(pt.ij)
            skip.ij <- rep(FALSE,nrow(pt.ij))
            tmp <- matrix("",nrow=nrow(pt.ij),ncol=maxncols.j)
            ii <- 1:ncol(pt.ij)
            tmp[,ii] <- pt.ij
            pt.j[[i]] <- pt.ij
            skip.j <- c(skip.j,skip.ij)
        }
        pt.j <- do.call(rbind,pt.j)
        
        if(length(sst)){
            tmp <- sst[[j]]
            sst.j <- matrix("",nrow=length(tmp),ncol=maxncols.j)
            sst.j[,1] <- tmp
            pt.j <- rbind(pt.j,sst.j)
            skip.j <- c(skip.j,rep(FALSE,length(tmp)))
        }

        pt.j <- apply(pt.j,2,centerAt,skip=skip.j)

        if(has.eq.headers){
            eq.header.j <- eq.headers[[name.j]]
            n.eq.j <- length(eq.header.j)
            eq.span <- ncol(pt.j)/n.eq.j
            dim(pt.j) <- c(nrow(pt.j),eq.span,n.eq.j)
            pt.j <- apply(pt.j,c(1,3),paste,collapse=colsep)
            pt.j <- rbind(eq.header.j,pt.j)
            pt.j <- apply(pt.j,2,format,justify="centre")
            pt.j <- apply(pt.j,1,paste,collapse=colsep)
        }
        else
            pt.j <- apply(pt.j,1,paste,collapse=colsep)
        res <- cbind(res,pt.j)
    }

    hlines <- character()
    if(l.headers){
        headers1 <- Map(structure,list(""),span=ncols) # To take care of multi-eqn models
        headers <- c(headers,list(headers1))
        headers <- rev(headers)
        hspan <- list()
        
        for(k in 1:length(headers)){
            hspan[[k]] <- sapply(headers[[k]],attr,"span")
        }
        hspan <- mkGrpSpan(hspan)
        headers <- headers[-1]
        hspan <- hspan[-1]

        for(k in 1:l.headers){

            headers.k <- headers[[k]]
            hspan.k <- hspan[[k]]
            res.grpd <- NULL
            cumsum <- 0
            
            for(j in 1:length(headers.k)){

                header.jk <- headers.k[[j]]
                hspan.jk <- hspan.k[[j]]
                res.cur.grp <- res[,cumsum + 1:hspan.jk,drop=FALSE]
                res.cur.grp <- apply(res.cur.grp,1,paste,collapse=colsep)
                nchar.jk <- nchar(res.cur.grp[1])
                if(has.eq.headers && k==1 || k > 1 && l.headers > 1){
                    header.sep <- mkRule(sectionsep,nchar.jk)
                    res.cur.grp <- c(header.sep,res.cur.grp)
                }
                res.cur.grp <- c(header.jk,res.cur.grp)
                res.cur.grp <- format(res.cur.grp,justify="centre")
                res.grpd <- cbind(res.grpd,res.cur.grp)

                cumsum <- cumsum + hspan.jk

            }
            res <- res.grpd
        }
    }
    res <- apply(res,2,function(x)paste0(" ",x))

    ld.headlines <- 0
    if(l.headers)
        ld.headlines <- ld.headlines + l.headers*2 - 1
    if(has.eq.headers){
        if(l.headers)
            ld.headlines <- ld.headlines + 2
        else
            ld.headlines <- ld.headlines + 1
    }
    l.leaders <- length(leaders)
    if(l.leaders){
        leaders <- c(list(headers=rep(list(structure("",span=1)),
                                          ld.headlines)),leaders)
        leaders <- lapply(leaders,ldxp)
        if(show.parmtypes){
            parmtypes <- rownames(x$parmtab)
            for(p in parmtypes){
                leaders[[p]] <- rbind(p,leaders[[p]])
            }
        }
        leaders <- do.call(rbind,leaders)
        leaders <- format(leaders,justify="left")
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,paste,collapse=colsep)
    res <- paste0(padding,res,padding)
    width <- nchar(res[1])
    toprule <- mkRule(topsep,width)
    sectionrule <- mkRule(sectionsep,width)
    bottomrule <- mkRule(topsep,width)

    csum <- ld.headlines

    sectsep.at <- integer()
    sectseps <- character()

    sec.hrules.at <- integer()
    sec.hrules <- character()
    
    leaders.gap <- mkRule(" ",nchar(leaders[i]))
    for(i in 1:nrow(pt)){
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
        csum <- csum + nrow(pt[[i,1]])
        if(show.parmtypes)
            csum <- csum + 1
    }
    if(length(sst) && any(sapply(sst,length)>0)){
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
    }
    
    res <- .insert(res,
                   c(sec.hrules.at,sectsep.at),
                   c(sec.hrules,   sectseps)
                   )
   
    res <- c(toprule,res,bottomrule)

    signif.symbols <- x$signif.symbols
    if(length(signif.symbols)){
        signif.template <- getOption("signif.symbol.print.template",
                                     signif.symbol.print.default.template)
        signif.symbols <- format_signif_print(signif.symbols,
                                              signif.template,
                                              width=nchar(bottomrule)-2*nchar(padding))
        signif.symbols <- paste0(padding,signif.symbols,padding)
        res <- c(res,signif.symbols)
    }
    res <- paste0(res,rowsep)
    return(res)
}

.insert <- function(x,where,what){

    l <- rep(list(NULL),length(x))

    what <- rep_len(what,length(where))

    use <- where > 0
    where <- where[use]
    what <- what[use]

    l[where] <- what
    y <- Map(c,x,l)
    unlist(y,recursive=FALSE)
}

.last <- function(x){
    l <- length(x)
    x[[l]]
}

format_signif_print <- function(syms,tmpl,width,dec="."){
    title <- tmpl[1]
    clps <- tmpl[3]
    tmpl <- tmpl[2]
    res <- title
    empty.title <- paste(rep(" ",nchar(title)),collapse="")
    
    ns <- length(syms)
    for(i in 1:ns){
        sym <- names(syms)[i]
        thrsh <- unname(syms[i])
        thrsh <- gsub(".",dec,thrsh,fixed=TRUE)
        res.i <- sub("$sym",sym,tmpl,fixed=TRUE)
        res.i <- sub("$val",thrsh,res.i,fixed=TRUE)
        if(i < ns)
            res.i <- paste0(res.i,clps)
        len <- length(res)
        res.l <- res[len]
        n.res.l <- nchar(res.l)
        n.res.i <- nchar(res.i)
        if(n.res.l+n.res.i <= width)
            res[len] <- paste0(res.l,res.i)
        else
            res <- c(res,paste0(empty.title,res.i))
    }
    res
}
signif.symbol.print.default.template <- c("Significance: ","$sym = p < $val","; ")

.row_prepend <- function(x,where,value){
    l <- length(where)
    stopifnot(l == length(value))
    where_ <- where + 1:l - 1
    y <- matrix("",nrow(x)+l,ncol(x))
    nr.y <- nrow(y)
    rows.y <- 1:nr.y
    other <- rows.y[!(rows.y %in% where_)]
    y[other,] <- x
    for(i in 1:l){
        wh.i <- where_[l]
        y[wh.i,] <- value[[i]]
    }
    y
}
.drop_last <- function(x) x[-length(x)]
