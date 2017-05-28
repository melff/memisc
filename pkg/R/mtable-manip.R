dim.mtable <- function(x){

    sapply(dimnames(x),length)
}

dimnames.mtable <- function(x){
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

"[.mtable" <- function(x, i, j, drop = FALSE){

    
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
    return(structure(y,class="mtable"))
}



##

combine_mtables <- function(...){
  
    args <- list(...)
    argnames <- names(args)
  
    allcompo <- unique(unlist(lapply(x,names)))
    nonparnames <- c("sumstat","contrasts","xlevels","call")
    partypes <- setdiff(allcompo,nonparnames)
    
}

c.mtable <- function(...) combine_mtables(...)
