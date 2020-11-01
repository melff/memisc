getSummary.clm <- function(obj,
                           alpha=.05,
                           ...){
  
  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
  else smry$nobs
  
  prmtable <- coef(smry)

  ## Move threshold parameters to end
  coefnames <- names(smry$beta)
  parnames <- rownames(prmtable)
  threshnames <- setdiff(parnames,coefnames)
  
  lower <- qnorm(p=alpha/2,mean=prmtable[,1],sd=prmtable[,2])
  upper <- qnorm(p=1-alpha/2,mean=prmtable[,1],sd=prmtable[,2])
  
  prmtable <- cbind(prmtable,lower,upper)
  
  colnames(prmtable) <- c("est","se","stat","p","lwr","upr")

  dn <- list(
    rownames(prmtable),
    c("est","se","stat","p","lwr","upr"),
    names(obj$model)[1]
  )
  dim(prmtable) <- c(dim(prmtable)[1],dim(prmtable)[2],1)
  dimnames(prmtable) <- dn  

  coef <- prmtable[coefnames,,,drop=FALSE]
  thresh <- prmtable[threshnames,,,drop=FALSE]

  ans <- list(coef = coef,
              Thresholds = thresh)
  
  null.model <- update(obj, .~1)
  
  ll <- logLik(obj)
  ll0 <- logLik(null.model)
  
  LR <- 2*(ll-ll0)
  df <- null.model$df.residual - smry$df.residual
  
  dev <- -2*ll
  
  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(2*ll0/N)
    
    Aldrich.Nelson <- LR/(LR+N)
    McFadden <- 1 - dev/(-2*ll0)
    Cox.Snell <- 1 - exp(-LR/N)
    Nagelkerke <- Cox.Snell/(1-L0.pwr)
  }
  else {
    LR <- NA
    df <- NA
    p <- NA
    Aldrich.Nelson <- NA
    McFadden <- NA
    Cox.Snell <- NA
    Nagelkerke <- NA
  }
  
  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
    LR             = LR,
    df         = df,
    p             = p,
    logLik        = ll,
    deviance      = dev,
    Aldrich.Nelson = Aldrich.Nelson,
    McFadden      = McFadden,
    Cox.Snell       = Cox.Snell,
    Nagelkerke    = Nagelkerke,
    AIC           = AIC,
    BIC           = BIC,
    N             = N
  )

  ans <- c(ans,
           list(sumstat=sumstat,
                contrasts=obj$contrasts,
                xlevels=smry$xlevels,
                call=obj$call))
  return(ans)
}

getSummary.clmm <- function(obj,
                           alpha=.05,
                           ...){
  
  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
       else nrow(obj$model)
  
  prmtable <- coef(smry)

  ## Move threshold parameters to end
  coefnames <- names(smry$beta)
  parnames <- rownames(prmtable)
  threshnames <- setdiff(parnames,coefnames)
  
  lower <- qnorm(p=alpha/2,mean=prmtable[,1],sd=prmtable[,2])
  upper <- qnorm(p=1-alpha/2,mean=prmtable[,1],sd=prmtable[,2])
  
  prmtable <- cbind(prmtable,lower,upper)
  
  colnames(prmtable) <- c("est","se","stat","p","lwr","upr")

  dn <- list(
    rownames(prmtable),
    c("est","se","stat","p","lwr","upr"),
    names(obj$model)[1]
  )
  dim(prmtable) <- c(dim(prmtable)[1],dim(prmtable)[2],1)
  dimnames(prmtable) <- dn  

  coef <- prmtable[coefnames,,,drop=FALSE]
  thresh <- prmtable[threshnames,,,drop=FALSE]

  ans <- list(coef = coef,
              Thresholds = thresh)

  varcor <- lapply(obj$ST,tcrossprod)

  VarPar <- NULL
    
  for(i in seq_along(varcor)){
    vc.i <- varcor[[i]]
    lv.i <- names(varcor)[i]
    vr.i <- diag(vc.i)
    cv.i <- vc.i[lower.tri(vc.i)]
    nms.i <- rownames(vc.i)
    nms.i <- gsub("(Intercept)","1",nms.i,fixed=TRUE)
    vrnames.i <- paste0("Var(~",nms.i,"|",lv.i,")")
    cvnames.i <- t(outer(nms.i,nms.i,FUN=paste,sep=":"))
    cvnames.i <- cvnames.i[lower.tri(cvnames.i)]
    if(length(cvnames.i))
      cvnames.i <- paste0("Cov(~",cvnames.i,"|",lv.i,")")
    vp.i <- matrix(NA,nrow=length(vr.i)+length(cv.i),ncol=6)
    vp.i[,1] <- c(vr.i,cv.i)
    dim(vp.i) <- c(dim(vp.i),1)
    dimnames(vp.i) <- list(c(vrnames.i,cvnames.i),
                           c("est","se","stat","p","lwr","upr"),
                           names(obj$model)[1])
    VarPar <- c(VarPar,structure(
                           list(vp.i),
                           names=lv.i))
 }
  ans <- c(ans,list(Variances=VarPar))
  
  # null.model <- update(obj, .~1)
  
  ll <- logLik(obj)
  # ll0 <- logLik(null.model)
  
  # LR <- 2*(ll-ll0)
  # df <- null.model$df.residual - smry$df.residual
  
  dev <- -2*ll
  
  # if(df > 0){
  #   p <- pchisq(LR,df,lower.tail=FALSE)
  #   L0.pwr <- exp(2*ll0/N)
  #   Aldrich.Nelson <- LR/(LR+N)
  #   McFadden <- 1 - dev/(-2*ll0)
  #   Cox.Snell <- 1 - exp(-LR/N)
  #   Nagelkerke <- Cox.Snell/(1-L0.pwr)
  # }
  # else {
    df <- NA
    LR <- NA
    p <- NA
    Aldrich.Nelson <- NA
    McFadden <- NA
    Cox.Snell <- NA
    Nagelkerke <- NA
  # }
  
  AIC <- AIC(obj)
  # BIC <- AIC(obj,k=log(N))
  sumstat <- c(
    # LR            = LR,
    # df            = df,
    # p             = p,
    logLik        = ll,
    deviance      = dev,
    # Aldrich.Nelson = Aldrich.Nelson,
    # McFadden      = McFadden,
    # Cox.Snell       = Cox.Snell,
    # Nagelkerke    = Nagelkerke,
    AIC           = AIC,
  #   BIC           = BIC,
    N             = N
  )

  ans <- c(ans,
           list(sumstat=sumstat,
                contrasts=obj$contrasts,
                xlevels=smry$xlevels,
                call=obj$call))
  return(ans)
}

