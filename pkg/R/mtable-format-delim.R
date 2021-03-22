hdxp0 <- function(x,span) {
    y <- matrix(rep("",span),nrow=1,ncol=span)
    y[,1] <- x
    y
}
hdxp1 <- function(x) {
    span <- attr(x,"span")
    y <- matrix(rep("",span),nrow=1,ncol=span)
    y[,1] <- x
    y
}
hdxp <- function(x)do.call(cbind,lapply(x,hdxp1))

ldxp1 <- function(x) {
    span <- attr(x,"span")
    y <- matrix(rep("",span),nrow=span,ncol=1)
    y[1,] <- x
    y
}
ldxp <- function(x)do.call(rbind,lapply(x,ldxp1))


mtable_format_delim <- function(x,
                                colsep="\t",
                                rowsep="\n",
                                interaction.sep = " x ",
                                ...)
    pf_mtable_format_delim(preformat_mtable(x),
                                colsep=colsep,
                                rowsep=rowsep,
                                interaction.sep = interaction.sep,
                                ...)

pf_mtable_format_delim <- function(x,
                                colsep="\t",
                                rowsep="\n",
                                interaction.sep = " x ",
                                ...
                                ){

    pt <- x$parmtab
    sst <- x$summary.stats
    sh <- x$sect.headers
    leaders <- x$leaders
    headers <- x$headers
    eq.headers <- x$eq.headers

    res <- NULL

    l.headers <- length(headers)
    l.leaders <- length(leaders)

    has.eq.headers <- length(eq.headers) > 0

    for(j in 1:ncol(pt)){
        
        name.j <- colnames(pt)[j]
        pt.j <- pt[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)

        pt.j <- do.call(rbind,pt.j)

        if(has.eq.headers){
            eq.header.j <- eq.headers[[name.j]]
            n.eq.j <- length(eq.header.j)
            eq.span <- ncol(pt.j)/n.eq.j
            tmp <- matrix("",ncol=n.eq.j,nrow=eq.span)
            tmp[1,] <- eq.header.j
            eq.header.j <- as.vector(tmp)
            pt.j <- rbind(eq.header.j,pt.j)
        }

        if(length(sst)){
            sst.j <- sst[[j]]
            sst.j <- colexpand(sst.j,ncol.j)
            pt.j <- rbind(pt.j,sst.j)
        }
        
        res <- cbind(res,pt.j)
    }

    if(l.headers){
        for(k in 1:l.headers){
            headers.k <- headers[[k]]
            headers.k <- lapply(headers.k,hdxp1)
            headers.k <- do.call(cbind,headers.k)
            headers[[k]] <- headers.k
        }
        headers <- do.call(rbind,headers)
        res <- rbind(headers,res)
    }
    

    if(l.leaders){
        lh <- l.headers + has.eq.headers      
        if(lh)
            leaders <- c(rep(list(list(structure("",span=1))),lh),
                         leaders)
        leaders <- lapply(leaders,ldxp)
        leaders <- do.call(rbind,leaders)
        
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,paste,collapse=colsep)
    res <- paste0(res,rowsep)
    return(res)
}
