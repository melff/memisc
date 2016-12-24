withVCov <- function(x,vcov){

    if(is.function(vcov))
        V <- vcov(x)
    else if(is.matrix(vcov))
        V <- vcov
    else
        stop("argument 'vcov' should be a matrix")

    cls <- class(x)
    cls <- c(paste("withVCov",cls[1],sep="."),
             "withVCov",
             class(x))
    structure(x,
              .VCov=V,
              class=cls)
}

summary.withVCov <- function(x){

    V <- attr(x,".VCov")
    
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

summary.withVCov.lm <- function(x){

    V <- attr(x,".VCov")
    
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
