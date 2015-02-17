dummy_labels <- function(x){
  ux <- sort(unique(as.character(x)))
  new("value.labels",ux,values=ux)
}


contr <- function(type,...){
  call <- match.call()
  contr.fun <- as.name(paste("contr",type,sep="."))
  args <- list(n=quote(n),...,contrasts=quote(contrasts))
  fun <- function(n,contrasts=TRUE) NULL
  body(fun) <- as.call(c(contr.fun,args))
  fun
}

setMethod("contrasts","ANY",function(x,contrasts=TRUE,...)stats::contrasts(x,contrasts=contrasts,...))

## Copied from stats:contrasts and modified
## Original copyright (C) 1995-2013 The R Core Team

setMethod("contrasts","item",function(x,contrasts=TRUE,...)
{
    if(measurement(x) %nin% c("nominal","ordinal"))
      warning("contrasts(x,...) called with non-categorical x")
    if(!length(vl <- labels(x))) vl <- dummy_labels(x) # stop("cannot obtain contrasts for unlabelled item")
    vl <- vl[is.valid2(vl@values,x@value.filter)]
    nvl <- length(vl@values)
    labs <- vl@.Data
    if (!contrasts)
        return(structure(diag(nvl), dimnames = list(labs,
                                                    labs)))
    ctr <- attr(x, "contrasts")
    if (is.null(ctr)) {
        ctrname <- getOption("contrasts")[[if (is.nominal(x)) 1 else 2]]
        ctr <- get(ctrname, mode = "function", envir = parent.frame())(labs,
                            contrasts = contrasts)
    }
    else if (is.character(ctr)){
        ctr <- get(ctr, mode = "function", envir = parent.frame())(labs,
                            contrasts = contrasts)
    }
    else if (is.function(ctr)){
        ctr <- ctr(labs,contrasts = contrasts)
    }
    else if (is.matrix(ctr) && nrow(ctr) != nvl){
        warning("contrast matrix has wrong rows, deleting it")
        ctr <- NULL
    }
    ctr
})

## copied from stats:contrasts<- and modified 
## Original copyright (C) 1995-2013 The R Core Team

setMethod("contrasts<-","item",function(x,how.many,value){
    if(measurement(x) %nin% c("nominal","ordinal"))
      warning("contrasts(x,...) called with non-categorical x")
    if(!length(vl <- labels(x))) vl <- dummy_labels(x) #stop("cannot obtain contrasts for unlabelled item")
    vl <- vl[is.valid2(vl@values,x@value.filter)]
    nvl <- length(vl@values)
    labs <- vl@.Data

    if (nvl < 2)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if (is.numeric(value)) {
        value <- as.matrix(value)
        if (nrow(value) != nvl)
            stop("wrong number of contrast matrix rows")
        n1 <- if (missing(how.many)) nvl - 1
              else how.many
        nc <- ncol(value)
        rownames(value) <- labs
        if (nc < n1) {
            cm <- qr(cbind(1, value))
            if (cm$rank != nc + 1)
                stop("singular contrast matrix")
            cm <- qr.qy(cm, diag(nvl))[, 2:nvl]
            cm[, 1:nc] <- value
            dimnames(cm) <- list(levels(x), NULL)
            if (!is.null(nmcol <- dimnames(value)[[2]]))
                dimnames(cm)[[2]] <- c(nmcol,
                              rep.int("", n1 - nc))
        }
        else cm <- value[, 1:n1, drop = FALSE]
    }
    else if (is.function(value))
        cm <- value
    else if (is.character(value))
        cm <- value
    else if (is.null(value))
        cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
})

## copied from stats:contr.treatment and modified
## Original copyright (C) 1995-2013 The R Core Team

contr.treatment <- function (n, base = 1, contrasts = TRUE)
{
    if (is.numeric(n) && length(n) == 1) {
        if (n > 1)
            levs <- 1:n
        else stop("not enough degrees of freedom to define contrasts")
    }
    else {
        levs <- n
        n <- length(n)
    }
    contr <- array(0, c(n, n), list(levs, levs))
    diag(contr) <- 1
    if (contrasts) {
        if (n < 2)
            stop(gettextf("contrasts not defined for %d degrees of freedom",
                n - 1), domain = NA)
        if (is.character(base)){
            base <- match(base,rownames(contr))
            if(is.na(base)) stop("Undefined baseline category")
        }
        if (base < 1 | base > n)
            stop("baseline group number out of range")
        contr <- contr[, -base, drop = FALSE]
    }
    contr
}

## copied from stats:contr.sum and modified
## Original copyright (C) 1995-2013 The R Core Team

contr.sum <- function (n, base = NULL, contrasts = TRUE)
{
    if (is.numeric(n) && length(n) == 1)
        levs <- 1:n
    else {
        levs <- n
        n <- length(n)
    }
    contr <- array(0, c(n, n), list(levs, levs))
    diag(contr) <- 1
    if (contrasts) {
        if (n < 2)
            stop(paste("Contrasts not defined for", n - 1, "degrees of freedom"))
        if (is.null(base)) base <- n
        if (is.character(base)){
            base <- match(base,rownames(contr))
            if(is.na(base)) stop("Undefined baseline category")
        }
        if (base < 1 | base > n)
            stop("Baseline group number out of range")
        contr <- contr[, -base, drop = FALSE]
        contr[base,] <- -1
    }
    contr
}

## copied from MASS:contr.sdif and modified
## Original copyright (C) Brian Ripley

contr.sdif <- function (n, contrasts = TRUE)
{
    if (is.numeric(n) && length(n) == 1) {
        if (n%%1 || n < 2)
            stop("invalid number of levels")
        lab <- as.character(seq(n))
    }
    else {
        lab <- as.character(n)
        n <- length(n)
        if (n < 2)
            stop("invalid number of levels")
    }
    if (contrasts) {
        contr <- col(matrix(nrow = n, ncol = n - 1))
        upper.tri <- !lower.tri(contr)
        contr[upper.tri] <- contr[upper.tri] - n
        structure(contr/n, dimnames = list(lab, paste(lab[-1],
            lab[-n], sep = "/")))
    }
    else structure(diag(n), dimnames = list(lab, lab))
}
