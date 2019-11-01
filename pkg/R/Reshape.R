
unlist1 <- function(x)unlist(as.list(x[-1]))

Reshape <- function(data,spec,direction){

    spec <- substitute(spec)
    spec <- as.list(spec[-1])
    spec1 <- lapply(spec,unlist1)
    vars <- sapply(spec1,is.list)
    v.names <- names(spec)[vars]
    varying <- lapply(spec1[vars],as.character)
    varying <- lapply(varying,function(x){
        x[!nzchar(x)] <- "__tmp__na__"
        return(x)
    })
    times <- spec[!vars]
    if(length(times)){
        times <- spec[!vars][[1]]
        times <- eval(times,parent.frame())
        timevar <- names(spec)[!vars]
    }
    else {
        n <- length(varying[[1]])
        times <- 1:n
        timevar <- "_t_"
    }
    data[["__tmp__na__"]] <- NA
    res <- reshape(data,
                   varying=varying,
                   v.names=v.names,
                   times=times,
                   timevar=timevar,
                   direction=direction)
    res["__tmp__na__"] <- NULL
    if(direction=="wide"){
        reshapeAttr <- attr(res,"reshapeLong")
        varying <- unlist(varying)
        othervars <- !(names(res) %in% varying)
        othervars <- names(res)[othervars]
        varying <- varying[varying %in% names(res)]
        res <- res[c(othervars,varying)]
        rownames(res) <- NULL
    }
    if(direction=="long"){
        if(is.character(times))
            res[[timevar]] <- factor(res[[timevar]],
                                      levels=times)
        idvar <- "id"
        ii <- order(res[[idvar]],res[[timevar]])
        res <- res[ii,]
    }
    res
}
