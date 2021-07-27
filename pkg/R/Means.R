each_term <- function(x){
   if(length(x) < 3)
       return(x)
   else
       c(Recall(x[[2]]),x[[3]])
}

dfTapply <- function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE) 
{
    FUN <- if (!is.null(FUN)) 
        match.fun(FUN)
    if (!is.list(INDEX)) 
        INDEX <- list(INDEX)
    INDEX <- lapply(INDEX, as.factor)
    nI <- length(INDEX)
    if (!nI) 
        stop("'INDEX' is of length zero")
    if (!all(lengths(INDEX) == NROW(X))) 
        stop("arguments must have same length")
    namelist <- lapply(INDEX, levels)
    extent <- lengths(namelist, use.names = FALSE)
    cumextent <- cumprod(extent)
    if (cumextent[nI] > .Machine$integer.max) 
        stop("total number of levels >= 2^31")
    storage.mode(cumextent) <- "integer"
    ngroup <- cumextent[nI]
    group <- as.integer(INDEX[[1L]])
    if (nI > 1L) 
        for (i in 2L:nI) group <- group + cumextent[i - 1L] * 
            (as.integer(INDEX[[i]]) - 1L)
    if (is.null(FUN)) 
        return(group)
    levels(group) <- as.character(seq_len(ngroup))
    class(group) <- "factor"
    ans <- split(X, group)
    names(ans) <- NULL
    index <- as.logical(lengths(ans))
    ans <- lapply(X = ans[index], FUN = FUN, ...)
    ansmat <- array(if (simplify && all(lengths(ans) == 1L)) {
        ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
        if (!is.null(ans) && is.na(default) && is.atomic(ans)) 
            vector(typeof(ans))
        else default
    }
    else vector("list", prod(extent)), dim = extent, dimnames = namelist)
    if (length(ans)) {
        ansmat[index] <- ans
    }
    ansmat
}

mean_se <- function(x,na.rm=FALSE){
    if(na.rm)
        x <- na.omit(x)
    n <- length(x)
    sqrt(var(x)/n)
}

nvalid <- function(x) length(na.omit(x))

WMean <- function(x,na.rm=TRUE){
    weighted.mean(x=x[,1],w=x[,2],na.rm=TRUE)
}

WMean_se <- function(x,w,na.rm=FALSE){
    if(na.rm){
        isn <- is.na(x) || is.na(w)
        x <- x[!isn]
        w <- w[!isn]
    }
    n <- sum(w)
    nn1 <- n/(n-1)
    wm.x <- weighted.mean(x,w)
    wvar.x <- weighted.mean((x-wm.x)^2)*nn1
    sqrt(wvar.x/n)
}

is.formula <- function(x) typeof(x) == "language" && class(x) == "formula"

Means.data.frame <-
    function(data,
             by,
             weights=NULL,
             subset = NULL,
             default = NA,
             se = FALSE,
             ci = FALSE,
             ci.level = 0.95,
             counts = FALSE,
             ...){
    if(is.formula(by)){
        formula <- by
        lhs <- formula[[2]]
        rhs <- formula[[3]]
        if(typeof(lhs) == "symbol"){
            nms <- deparse(lhs)
        }
        else if(deparse(lhs[[1]]) != "cbind"){
          lhs <- each_term(lhs)
          nms <- sapply(lhs,deparse)
          lhs <- as.call(c(quote(cbind),lhs))
          formula[[2]] <- lhs
        }
        else {
            if(length(names(lhs)))
                nms <- names(lhs)[-1]
            else
                nms <- NULL
        }
        mf <- match.call(expand.dots=FALSE)
        m <- match(c("data", "subset", "weights"), names(mf), 0L)
        mf <- mf[c(1L, m)]
        mf$drop.unused.levels <- TRUE
        mf$na.action <- quote(na.pass)
        mf$formula <- formula
        mf[[1L]] <- quote(stats::model.frame)

        mf <- eval(mf, parent.frame())
        if(!missing(weights)){
            weights <- model.weights(mf)
            mf["(weights)"] <- NULL
        }
        rsp <- mf[[1]]
        rsp <- as.data.frame(rsp)
        names(rsp) <- nms
        if(identical(rhs,1)){
            fcts <- rep(1,nrow(data))
            rhs_const <- TRUE
        }
        else{
            fcts <- mf[-1]
            rhs_const <- FALSE
        }
    }
    else if(is.character(by)){
        fcts <- data[by]
        rsp <- !(names(data) %in% by)
        rsp <- data[rsp]
        rhs_const <- FALSE
    }
    else if(is.data.frame(by)){
        rsp <- !(names(data) %in% names(by))
        fcts <- by
        rsp <- data[rsp]
        rhs_const <- FALSE
    }
    else if(is.numeric(by) && length(by)==1){
        fcts <- rep(1,nrow(data))
        rsp <- data
        rhs_const <- TRUE
    } else {
        fcts <- by
        rsp <- data
        rhs_const <- FALSE
    }
    if(!rhs_const && !all(sapply(fcts,is.factor))) stop("all grouping variables must be factors")
    nms <- names(rsp)
    res <- vector(mode="list",length=ncol(rsp))
    means <- res
    if(missing(weights) || !length(weights)){
        for(i in 1:ncol(rsp)){
            means[[i]] <- tapply(rsp[[i]],fcts,mean,na.rm=TRUE,default=default)
        }
    }
    else{
        for(i in 1:ncol(rsp)){
            rsp_i <- data.frame(rsp[[i]],weights)
            means[[i]] <- dfTapply(rsp_i,fcts,WMean,na.rm=TRUE,default=default)
        }
    }
    names(means) <- nms
    means <- simplify2array(means)
    if(rhs_const) names(means) <- nms
    do_se <- se
    do_se <- do_se || ci
    do_counts <- counts || ci
    if(do_counts){
        n <- res
        for(i in 1:ncol(rsp)){
            n[[i]] <- tapply(rsp[[i]],fcts,nvalid,default=default)
        }
        names(n) <- nms
        n <- simplify2array(n)
    }
    if(do_se){
        mean_se <- res
        for(i in 1:ncol(rsp)){
            mean_se[[i]] <- tapply(rsp[[i]],fcts,mean_se,na.rm=TRUE,default=default)
        }
        names(mean_se) <- nms
        mean_se <- simplify2array(mean_se)
    }
    if(ci){
        alpha <- 1-ci.level
        tval <- qt(1-alpha/2,df=n-1)
        se_tval <- mean_se*tval
        Lower <- means - se_tval
        Upper <- means - se_tval
        ConfInt <- list(Lower = Lower,
                        Upper = Upper)
    }
    res <- list(Mean = means)
    if(se){
        res <- c(res,list(SE = mean_se))
    }
    if(ci){
        res <- c(res,ConfInt)
    }
    if(counts){
        res <- c(res,list(N = n))
    }
    if(se || ci || counts){
        if(rhs_const)
                res <- do.call(cbind,res)
            else
                res <- simplify2array(res)
        ldims <- length(dim(res))
        if(ldims > 1){
            names(dimnames(res))[ldims-1] <- "Variable"
            names(dimnames(res))[ldims] <- "Statistic"
        }
        structure(res,class=c("xmeans.table","means.table","table"),
                  rhs_const=rhs_const)
    }
    else{
        res <- res$Mean
        if(!is.array(res)){
            res <- as.matrix(res)
            colnames(res) <- "Mean"
        }
        structure(res,class=c("means.table","table"),
                  rhs_const=rhs_const)
    }
}

Means.numeric <- function(data,...){
    data <- eval(substitute(data.frame(data)),parent.frame())
    cl <- match.call()
    cl$data <- data
    cl[[1]] <- as.symbol("Means")
    eval(cl,parent.frame())
}

Means.data.set <- function(data,...){
    cl <- match.call()
    data <- as.data.frame(data)
    cl$data <- data
    cl[[1]] <- as.symbol("Means")
    eval(cl,parent.frame())
}

Means.formula <- function(data,subset,weights,
                          ...){
    formula <- data
    cl <- mf <- match.call(expand.dots=FALSE)
    m <- match(c("subset", "weights"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf$formula <- formula
    mf[[1L]] <- quote(stats::model.frame)
    data <- eval(mf, parent.frame())
    by <- formula
    cl$data <- data
    cl$by <- by
    cl[[1]] <- as.symbol("Means")
    eval(cl,parent.frame())
}

Means <- function(data,...) UseMethod("Means")

as.data.frame.means.table <- function(x, row.names=NULL, optional=TRUE, drop=TRUE, ...){
    rhs_const <- attr(x,"rhs_const")
    ldims <- length(dim(x))
    if(rhs_const)
        res <- as.data.frame.matrix(x)
    else
        res <- to.data.frame(x,as.vars=ldims)
    if(drop)
        na.omit(res)
    else res
}

as.data.frame.xmeans.table <- function(x, row.names=NULL, optional=TRUE, drop=TRUE, ...){
    rhs_const <- attr(x,"rhs_const")
    ldims <- length(dim(x))
    if(rhs_const)
        res <- to.data.frame(x,as.vars=2)
    else
        res <- to.data.frame(x,as.vars=ldims)
    if(drop)
        na.omit(res)
    else res
}
