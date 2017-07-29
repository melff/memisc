dim.memisc_mtable <- function(x){

    sapply(dimnames(x),length)
}

dimnames.memisc_mtable <- function(x){

    coln <- names(x)

    allcompo <- unique(unlist(lapply(x,names)))
    nonparnames <- c("sumstat","contrasts","xlevels","call")
    partypes <- setdiff(allcompo,nonparnames)

    parms <- lapply(x,`[`,partypes)
    parms <- do.call(cbind,parms)

    rown <- lapply(1:nrow(parms),function(i)unique(unlist(lapply(parms[i,],rownames))))

    rown <- Map(
        function(x,n)
        {
            names(x) <- rep(n,length(x))
            x
        },rown,partypes)
    
    rown <- do.call(c,rown)
    
    list(
        rown,
        coln
    )
}

"[.memisc_mtable" <- function(x, i, j, drop = FALSE){

    
    dn.x <- dimnames(x)
    rown <- dn.x[[1]]
    coln <- dn.x[[2]]

    allcompo <- unique(unlist(lapply(x,names)))
    nonparnames <- c("sumstat","contrasts","xlevels","call")
    partypes <- setdiff(allcompo,nonparnames)
    
    nrows <- length(rown)
    ncols <- length(coln)
    
    mdrop <- missing(drop)
    Narg <- nargs() - (!mdrop)
    
    if(Narg<3){
        
        if(missing(i)){
            
            i <- 1:nrows
            j <- 1:ncols
        }
        else {
            
            j <- i
            i <- 1:nrows
        }
    }
    else {
        
        if(missing(i)) i <- 1:nrows
        if(missing(j)) j <- 1:ncols
    }

    if(is.logical(i))
        i <- unique(rown[i])
    else if(is.numeric(i)){
        i <- unique(rown[i])
    }
    else if(!is.character(i)){
        stop("wrong index type ",typeof(i))
    }

    if(is.logical(j))
        j <- which(j)
    else if(is.character(j)){
        j <- match(j,names(x))
    }
    else if(!is.numeric(j)){
                stop("wrong index type ",typeof(i))
    }
    
    y <- unclass(x)[j]
    attr.x <- attributes(x)
    attr.x$names <- attr.x$names[j]
    attr.x$stemplates <- attr.x$stemplates[j]
    
    for(pt in partypes){
        for(m in 1:length(y)){
            tmp <- y[[m]][[pt]]
            i.tmp <- intersect(i,rownames(tmp))
            if(length(i.tmp))
                y[[m]][[pt]] <- tmp[i.tmp,,drop=FALSE]
            else
                y[[m]][[pt]] <- NULL
        }
    }

    attributes(y) <- attr.x
    return(structure(y,class="memisc_mtable"))
}



##
get.summary.stats <- function(x){
    y <- attr(x,"summary.stats")
    if(is.logical(y)){
        if(isTRUE(y)){
           names(unlist(attr(x,"stemplates")))
        }
        else character()
    }
    else
        y
}

combine_mtables <- function(...){
  
    args <- list(...)
    argnames <- names(args)
    names(args) <- NULL

    if(length(argnames)>1){
        
        mg <- sapply(args,length)   
        mg1 <- lapply(mg,seq.int)
        mg0 <- c(0,mg[-length(mg)])
        model.groups <- Map(`+`,mg0,mg1)
        names(model.groups) <- argnames
    }
    else
        model.groups <- NULL
    
    coef.style <- attr(args[[1]],"coef.style")
    
    summary.stats <- lapply(args,get.summary.stats)
    summary.stats <- unique(unlist(summary.stats))

    signif.symbols <- attr(args[[1]],"signif.symbols")       
    factor.style <- attr(args[[1]],"factor.style")
    show.baselevel <- attr(args[[1]],"show.baselevel")
    baselevel.sep <- attr(args[[1]],"baselevel.sep")
    float.style <- attr(args[[1]],"float.style")
    digits <- attr(args[[1]],"digits")
    stemplates <- do.call(c,lapply(args,attr,"stemplates"))
    sdigits <- attr(args[[1]],"sdigits")

    args <- lapply(args,unclass)

    res <- do.call(c,args)

    structure(res,
            class="memisc_mtable",
            coef.style=coef.style,
            summary.stats=summary.stats,
            signif.symbols=signif.symbols,
            factor.style=factor.style,
            show.baselevel=show.baselevel,
            baselevel.sep=baselevel.sep,
            float.style=float.style,
            digits=digits,
            stemplates=stemplates,
            sdigits=sdigits,
            model.groups=model.groups
            )
}

c.memisc_mtable <- function(...) combine_mtables(...)
