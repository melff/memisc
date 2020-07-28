rabind2 <- function(x,y){
    if(!length(x)) return(y)
    else if(!length(y)) return(x)
    else {
        rnx <- rownames(x)
        rny <- rownames(y)
        nrx <- nrow(x)
        nry <- nrow(y)
        dimx <- dim(x)[-1]
        dimy <- dim(y)[-1]
        stopifnot(all(dimx==dimy))
        dimnx <- dimnames(x)[-1]
        dimny <- dimnames(y)[-1]
        x <- array(x,dim=c(nrx,prod(dimx)))
        y <- array(y,dim=c(nry,prod(dimy)))
        z <- rbind(x,y)
        dim(z) <- c(nrx+nry,dimx)
        dimnames(z) <- c(list(c(rnx,rny)),
                         dimnx)
        return(z)
    }
}
