mtable_format_delim <- function(x,...)
    pf_mtable_format_delim(preformat_mtable(x),...)

pf_mtable_format_delim <- function(x,
                                colsep="\t",
                                rowsep="\n",
                                interaction.sep = " x ",
                                force.names = FALSE,
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
                if(length(sh.ij)){
                    sh.ij <- hdxp(sh.ij,span.j)
                    
                    pt.ij <- rbind(sh.ij,pt.ij)
                }
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
            header.j <- hdxp(list(header.j))
            pt.j <- rbind(header.j,pt.j)
        }
        
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
        
        res <- cbind(leaders,res)
    }

    res <- apply(res,1,paste,collapse=colsep)
    res <- paste0(res,rowsep)
    return(res)
}
