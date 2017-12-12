dim.memisc_mtable <- function(x){

    sapply(dimnames(x),length)
}

dimnames.memisc_mtable <- function(x){

    modn <- names(x)

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

    parn <- attr(x,"parameter.names")
    
    resp <- unique(unlist(lapply(parms[1,],dimnames3)))
    
    list(
        parn,
        resp,
        modn
    )
}

safe.charidx <- function(nms,i){

    lnms <- length(nms)
    if(!length(i))
        i <- nms
    else if(is.character(i))
        i <- i[i %in% nms]
    else if(is.logical(i))
        i <- nms[i]
    else if(is.numeric(i)){
        i <- i[abs(i) <= lnms]
        i <- nms[i]
    }
    i
}

"[.memisc_mtable" <- function(x, i, j, k){

    if(nargs() == 2){
        k <- if(missing(i)) return(x) else i
        i <- logical(0) 
        j <- logical(0) 
    }
    else if(nargs() == 3){
        i <- if(missing(i)) logical(0) else i
        k <- if(missing(j)) logical(0) else j
        j <- logical(0)
    }
    else { #nargs() == 4   
        i <- if(missing(i)) logical(0) else i
        j <- if(missing(j)) logical(0) else j
        k <- if(missing(k)) logical(0) else k
    }
    
    dn.x <- dimnames(x)
    parn <- dn.x[[1]]
    resp <- dn.x[[2]]
    modn <- dn.x[[3]]
        
    allcompo <- unique(unlist(lapply(x,names)))
    nonparnames <- c("sumstat","contrasts","xlevels","call")
    partypes <- setdiff(allcompo,nonparnames)
    
    nparn <- length(parn)
    nresp <- length(resp)
    nmodn <- length(modn)

    i <- safe.charidx(parn,i)
    j <- safe.charidx(resp,j)
    k <- safe.charidx(modn,k)
    kk <- match(k,names(x))
    
    y <- unclass(x)[k]
    attr.y <- attributes(x)
    attr.y$names <- k
    attr.y$stemplates <- attr.y$stemplates[kk]
    attr.y$parameter.names <- i

    l.y <- length(y)
    l.pt <- length(partypes)

    lt <- matrix(0,l.pt,l.y)
    
    for(k in 1:l.pt){
        pt <- partypes[k]
        for(m in 1:l.y){
            tmp <- y[[m]][[pt]]
            i.tmp <- i[i %in% rownames(tmp)]
            j.tmp <- j[j %in% dimnames3(tmp)]
            if(length(i.tmp) && length(j.tmp))
                tmp <- tmp[i.tmp,,j.tmp,drop=FALSE]
            else if(length(i.tmp))
                tmp <- tmp[i.tmp,,0,drop=FALSE]
            else if(length(j.tmp))
                tmp <- tmp[0,,j.tmp,drop=FALSE]
            else
                tmp <- tmp[0,,0,drop=FALSE]
            y[[m]][[pt]] <- tmp
            lt[k,m] <- length(tmp)
        }
        if(!any(lt>0)){
            for(m in 1:l.y)
                y[[m]][[pt]] <- NULL
        }
    }
    if(all(lt==0))
        return(NULL)
    else{
        attributes(y) <- attr.y
        return(structure(y,class="memisc_mtable"))
    }
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

    parameter.names <- lapply(args,attr,"parameter.names")
    parameter.names <- unique(unlist(parameter.names))
    
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
    show.eqnames <- attr(args[[1]],"show.eqnames")

    args <- lapply(args,unclass)

    res <- do.call(c,args)

    structure(res,
            class="memisc_mtable",
            parameter.names=parameter.names,
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
            show.eqnames=show.eqnames,
            model.groups=model.groups
            )
}

c.memisc_mtable <- function(...) combine_mtables(...)
