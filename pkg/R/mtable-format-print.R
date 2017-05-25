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



ldxp1 <- function(x) {
    span <- attr(x,"span")
    y <- matrix(rep("",span),nrow=span,ncol=1)
    y[1,] <- x
    y
}
ldxp <- function(x)do.call(rbind,lapply(x,ldxp1))

hdxp1 <- function(x) {
    span <- attr(x,"span")
    y <- matrix(rep("",span),nrow=1,ncol=span)
    y[,1] <- x
    y
}
hdxp <- function(x)do.call(cbind,lapply(x,hdxp1))

mtable_format_print <- function(x,...)
    pf_mtable_format_print(preformat_mtable(x),...)

pf_mtable_format_print <- function(x,
                                   topsep="=",
                                   bottomsep="=",
                                   sectionsep="-",
                                   interaction.sep = " x ",
                                   center.at=getOption("OutDec"),
                                   align.integers=c("dot","right","left"),
                                   padding="  ",
                                   force.names = FALSE,
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
    
    for(j in 1:ncol(pt)){
        
        pt.j <- pt[,j]
        sh.j <- sh[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        span.j <- unique(sapply(sh.j,attr,"span"))

        if(is.numeric(span.j))
            pt.j <- lapply(pt.j,cols_folded,span.j,sep=colsep)

        max.width <- 0
        for(i in 1:length(pt.j)){
            pt.ij <- pt.j[[i]]
            pt.ij <- centerAt(pt.ij,at=center.at,integers=align.integers)
            #pt.ij <- format(pt.ij,justify="centre")
            if(need.sh[[i]]){
                sh.ij <- sh.j[[i]]
                if(length(sh.ij)){
                    sh.ij <- set_length(sh.ij,ncol.j/span.j)
                    #sh.ij <- format(sh.ij,justify="centre")
                    pt.ij <- rbind(sh.ij,pt.ij)
                }
                else
                    pt.ij <- rbind("",pt.ij)
            }
            #browser()
            pt.ij <- apply(pt.ij,2,format,justify="centre")
            pt.ij <- apply(pt.ij,1,paste,collapse=colsep)
            max.width <- max(max.width,nchar(pt.ij[1]))
            pt.j[[i]] <- pt.ij
        }

        pt.j <- lapply(pt.j,format,width=max.width+1)
        pt.j <- unlist(pt.j)
        if(length(sst)){
            sst.j <- sst[[j]]
            sst.j <- centerAt(sst.j,at=center.at,integers=align.integers)
            pt.j <- c(pt.j,sst.j)
        }

        if(length(headers)){
            header.j <- headers[[j]]
            if(length(header.j)){
                hspan.j <- attr(header.j,"span")
                stopifnot(hspan.j==ncol.j) # TODO: deal with extra-wide heders
            }
            else {
                header.j <- ""
                hspan.j <- ncol.j
            }
            pt.j <- c(header.j,pt.j)
        }
        
        
        pt.j <- format(pt.j,justify="centre")
        res <- cbind(res,pt.j)
    }

    if(length(leaders)){

        for(i in 1:length(leaders)){
            if(need.sh[i]){
                leaders.i <- leaders[[i]]
                leaders[[i]] <- c(list(structure("",span=1)),leaders.i)
            }
        }
        
        if(length(headers))
            leaders <- c(list(headers=list(structure("",span=1))),leaders)
# TODO: deal with multiline headers
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

    sect.at <- integer()
    csum <- 0
    for(i in 1:nrow(pt)){
        if(need.sh[i]){
            csum <- csum + 1
            sect.at <- c(sect.at,csum)
        }
        csum <- csum + nrow(pt[[i,1]])
        sect.at <- c(sect.at,csum)
    }
    if(length(headers))
        sect.at <- c(1,sect.at + 1)
    res <- .insert(res,sect.at,list(sectionrule))
    
    res <- c(toprule,res,bottomrule)
    res <- paste0(res,rowsep)
    return(res)
}

.insert <- function(x,where,what){

    l <- rep(list(NULL),length(x))
    l[where] <- what
    y <- Map(c,x,l)
    unlist(y,recursive=FALSE)
}

.last <- function(x){
    l <- length(x)
    x[[l]]
}
