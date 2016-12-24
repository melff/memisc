withSE <- function(object, vcov, ...){
    if(is.matrix(vcov)) UseMethod("withVCov")
    else if(is.function(vcov)) UseMethod("withVCov")
    else if(is.character(vcov)){
        if(exists(vcov))
            vcov <- get(vcov)
        else if(exists(vcov2 <- paste0("vcov",vcov)))
            vcov <- get(vcov2)
        else
            stop("neither '",vcov,"' nor '", vcov2, "' is a known function")
        withVCov(object, vcov=vcov, ...)
    }
    else stop("cannot handle 'vcov' argument of type",typeof(vcov))
}

withVCov <- function(object, vcov, ...) UseMethod("withVCov")

withVCov.lm <- function(object, vcov, ...){

    if(is.function(vcov))
        V <- vcov(object, ...)
    else if(is.matrix(vcov))
        V <- vcov
    else
        stop("argument 'vcov' should be a matrix")

    cls <- class(object)
    cls <- c(paste("withVCov",cls[1],sep="."),
             "withVCov",
             class(object))
    structure(object,
              .VCov=V,
              class=cls)
}

vcov.withVCov <- function(object, ...) attr(object,".VCov")

summary.withVCov <- function(object, ...){

    V <- attr(object,".VCov")
    
    res <- NextMethod()
    coefTab <- res$coefficients

    est <- coefTab[,1]
    se <- sqrt(diag(V))
    zval <- est/se
    pval <- 2 * pnorm(abs(zval),lower.tail=FALSE)
    coefTab[,2] <- se
    coefTab[,3] <- zval
    coefTab[,4] <- pval
    res$coefficients <- coefTab

    res
}

summary.withVCov.lm <- function(object, ...){

    V <- attr(object,".VCov")
    
    res <- NextMethod()
    coefTab <- res$coefficients
    rdf <- res$df[2]
    
    est <- coefTab[,1]
    se <- sqrt(diag(V))
    tval <- est/se
    pval <- 2 * pt(abs(tval),df=rdf,lower.tail=FALSE)
    coefTab[,2] <- se
    coefTab[,3] <- tval
    coefTab[,4] <- pval
    res$coefficients <- coefTab

    res
}
