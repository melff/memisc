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

    sh.nonnull <- !Sapply(sh,is.null)
    need.sh <- apply(sh.nonnull,1,any)
    if(length(sst))
        need.sh <- c(need.sh,FALSE)
    res <- NULL

    l.headers <- length(headers)
    l.leaders <- length(leaders)
    
    for(j in 1:ncol(pt)){
        
        pt.j <- pt[,j]
        sh.j <- sh[,j]

        ncol.j <- unique(sapply(pt.j,ncol))
        stopifnot(length(ncol.j)==1)
        span.j <- unique(sapply(sh.j,attr,"span"))

        for(i in 1:length(pt.j)){
            pt.ij <- pt.j[[i]]
            if(need.sh[[i]]){
                sh.ij <- sh.j[[i]]
                if(length(sh.ij))
                    pt.ij <- rbind(sh.ij,pt.ij)
                else
                    pt.ij <- rbind("",pt.ij)
            }
            pt.j[[i]] <- pt.ij
        }
        pt.j <- do.call(rbind,pt.j)
        
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

        for(i in 1:l.leaders){
            if(need.sh[i]){
                leaders.i <- leaders[[i]]
                leaders[[i]] <- c(list(structure("",span=1)),leaders.i)
            }
        }
        
        if(l.headers)
            leaders <- c(rep(list(list(structure("",span=1))),l.headers),
                         leaders)
        leaders <- lapply(leaders,ldxp)
        leaders <- do.call(rbind,leaders)
        
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,paste,collapse=colsep)
    res <- paste0(res,rowsep)
    return(res)
}
