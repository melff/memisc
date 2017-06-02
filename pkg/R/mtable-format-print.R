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
                                   ...
                                   ){

    colsep <- " "
    rowsep <- "\n"

    pt <- x$parmtab
    sst <- x$summary.stats
    sh <- x$sect.headers
    leaders <- x$leaders
    headers <- x$headers

    sh.nonnull <- !Sapply(sh,is.null)
    need.sh <- apply(sh.nonnull,1,any)
    if(length(sst))
        need.sh <- c(need.sh,FALSE)
    res <- NULL
    ncols <- NULL
    sec.hrule <- array(list(),dim=dim(sh))

    for(j in 1:ncol(pt)){
        
        pt.j <- pt[,j]
        sh.j <- sh[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        ncols <- c(ncols,ncol.j)

        span.j <- unique(sapply(sh.j,attr,"span"))

        if(is.numeric(span.j))
            pt.j <- lapply(pt.j,cols_folded,span.j,sep=colsep)

        max.width <- 0
        for(i in 1:length(pt.j)){
            pt.ij <- pt.j[[i]]
            pt.ij <- centerAt(pt.ij,at=center.at,integers=align.integers)
            if(need.sh[[i]]){
                sh.ij <- sh.j[[i]]
                if(length(sh.ij)){
                    sh.ij <- set_length(sh.ij,ncol.j/span.j)
                    pt.ij <- rbind(sh.ij,pt.ij)
                }
                else
                    pt.ij <- rbind("",pt.ij)
            }
            pt.ij <- apply(pt.ij,2,format,justify="centre")
            pt.ij <- apply(pt.ij,1,paste,collapse=colsep)
            max.width <- max(max.width,nchar(pt.ij[1]))
            pt.j[[i]] <- pt.ij
            
            if(need.sh[[i]]){
                if(length(sh.ij))
                    sec.hrule[i,j] <- mkRule(sectionsep,nchar(pt.ij[1]))
                else
                    sec.hrule[i,j] <- mkRule(" ",nchar(pt.ij[1]))
            }
        }

        pt.j <- lapply(pt.j,format,width=max.width+1)
        pt.j <- unlist(pt.j)
        if(length(sst)){
            sst.j <- sst[[j]]
            sst.j <- centerAt(sst.j,at=center.at,integers=align.integers)
            pt.j <- c(pt.j,sst.j)
        }

        pt.j <- format(pt.j,justify="centre")
        res <- cbind(res,pt.j)
    }

    l.headers <- length(headers)
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
                res.cur.grp <- c(header.jk,res.cur.grp)
                res.cur.grp <- format(res.cur.grp,justify="centre")
                res.grpd <- cbind(res.grpd,res.cur.grp)

                cumsum <- cumsum + hspan.jk

            }
            if(k>1){
                nn <- sapply(res.grpd[1,],nchar)
                hl <- Map(mkRule,list(sectionsep),nn)
                hlines[k-1] <- paste(unlist(hl),collapse=colsep)
            }
            res <- res.grpd
        }
    }

    l.leaders <- length(leaders)
    if(l.leaders){

        for(i in 1:l.leaders){
            if(need.sh[i]){
                leaders.i <- leaders[[i]]
                leaders[[i]] <- c(list(structure("",span=1)),leaders.i)
            }
        }
        
        if(l.headers)
            leaders <- c(list(headers=rep(list(structure("",span=1)),
                                          l.headers)),
                         leaders)

        leaders <- lapply(leaders,ldxp)
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

    csum <- l.headers

    sectsep.at <- integer()
    sectseps <- character()

    sec.hrules.at <- integer()
    sec.hrules <- character()
    
    leaders.gap <- mkRule(" ",nchar(leaders[i]))
    for(i in 1:nrow(pt)){
        if(need.sh[i]){
            sec.hrule.i <- do.call(paste,c(leaders.gap,sec.hrule[i,],sep=colsep))
            sec.hrule.i <- paste0(padding,sec.hrule.i,padding,colsep)
            sec.hrules    <- c(sec.hrules,    sec.hrule.i)
            sec.hrules.at <- c(sec.hrules.at, csum)
            csum <- csum + 1
        }
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
        csum <- csum + nrow(pt[[i,1]])
    }
    if(length(sst)){
        sectseps   <- c(sectseps,   sectionrule)
        sectsep.at <- c(sectsep.at, csum)
    }

    res <- .insert(res,
                   c(sec.hrules.at,sectsep.at),
                   c(sec.hrules,   sectseps)
                   )
    
    if(l.headers){
        #lh21 <- l.headers*2-1
        if(l.headers > 1){
            lwdth <- nchar(leaders[1])
            llh <- mkRule(" ",lwdth)
            hlines <- paste(llh,hlines,sep=colsep)
            hlines <- paste0(padding,hlines,padding)
            res <- .insert(res,1:length(hlines),hlines)
        }
    }
    
    res <- c(toprule,res,bottomrule)
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
