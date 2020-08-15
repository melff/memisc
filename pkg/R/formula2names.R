formula2coefs <- function(obj,fo,warn=FALSE){
    if(length(fo)>2)
        rhs <- fo[-2]
    else
        rhs <- fo
    xlevels <- get_xlevels(obj)
    tm <- terms(rhs)
    termlabs <- attr(tm,"term.labels")
    o.tm <- terms(obj)
    o.termlabs <- attr(o.tm,"term.labels")
    if(!all(termlabs%in%o.termlabs)){
        sdf <- setdiff(termlabs,o.termlabs)
        if(warn)
            warning(paste(paste(sdf,collapse=", "),"not in model"))
        termlabs <- intersect(termlabs,o.termlabs)
    }
    names <- lapply(termlabs,tl2coefs,xlevels=xlevels)
    names <- unlist(names)
    names <- unique(names)
    return(names)
}

formula2termlabs <- function(obj,fo,warn=FALSE){
    if(length(fo)>2)
        rhs <- fo[-2]
    else
        rhs <- fo
    tm <- terms(rhs)
    termlabs <- attr(tm,"term.labels")
    o.tm <- terms(obj)
    o.termlabs <- attr(o.tm,"term.labels")
    if(!all(termlabs%in%o.termlabs)){
        sdf <- setdiff(termlabs,o.termlabs)
        if(warn)
            warning(paste(paste(sdf,collapse=", "),"not in model"))
        termlabs <- intersect(termlabs,o.termlabs)
    }
    return(termlabs)
}


tl2coefs <- function(tl,xlevels){
    tl <- unlist(strsplit(tl,":"))
    nms <- lapply(tl,tl2coefs_,xlevels=xlevels)
    if(length(tl)>1)
        nms <- Reduce(intercoefs,nms)
    return(nms)
}

tl2coefs_ <- function(tl,xlevels){
    if(!(tl %in% names(xlevels)))
        return(tl)
    else {
        levs <- xlevels[[tl]]
        return(paste0(tl,levs))
    }
}

intercoefs <- function(x,y){
    outer(x,y,paste,sep=":")
}

get_xlevels <- function(obj) {
    if("xlevels" %in% names(obj))
        obj$xlevels
    else {
        xlevels <- list()
        Contr <- names(attr(model.matrix(obj), "contrasts"))
        for (c in Contr) xlevels[[c]] <- levels(obj@frame[,c])
        xlevels
    }
}
