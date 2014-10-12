## The following three functions are contributed by
## Christopher N. Lawrence, Ph.D. <c.n.lawrence@gmail.com>
## Assistant Professor of Political Science
## Texas A&M International University
## 313 LBVSC, 5201 University Blvd
## Laredo, Texas 78041-1920

## Some patches were applied by ME in July 2012

getSummary.polr <- function(obj,
            alpha=.05,
            ...){

  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
    else nobs(obj)

  coef <- smry$coef

  if (smry$df.residual) {
      pvals <- 2*pt(-abs(coef[,3]), smry$df.residual)
  } else {
      pvals <- 2*pnorm(-abs(coef[,3]))
  }

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,pvals,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  null.model <- update(obj, .~1)

  LR <- deviance(null.model) - deviance(obj)
  df <- null.model$df.residual - smry$df.resid

  ll <- logLik(obj)
  dev <- deviance(obj)

  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(-deviance(null.model)/N)
    #LM.pwr <- exp(-smry$deviance/N)

    Aldrich.Nelson <- LR/(LR+N)
    McFadden <- 1 - dev/deviance(null.model)
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

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=smry$xlevels,call=obj$call)
}

getSummary.clm <- function(obj,
                           alpha=.05,
                           ...){
  
  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
  else smry$nobs
  
  cf <- coef(smry)
  
  ## Move threshold parameters to end
  thresholds <- names(obj$xi)
  parameters <- rownames(cf)
  cf <- rbind(cf[setdiff(parameters, thresholds),], cf[thresholds,])
  
  lower <- qnorm(p=alpha/2,mean=cf[,1],sd=cf[,2])
  upper <- qnorm(p=1-alpha/2,mean=cf[,1],sd=cf[,2])
  
  cf <- cbind(cf,lower,upper)
  
  colnames(cf) <- c("est","se","stat","p","lwr","upr")
  #null.model <- update(obj, location=paste(names(obj$model[1]), " ~ 1"))
  null.model <- update(obj, .~1)
  
  ll <- logLik(obj)
  ll0 <- logLik(null.model)
  
  LR <- 2*(ll-ll0)
  df <- null.model$df.residual - smry$df.residual
  
  dev <- -2*ll
  
  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(2*ll0/N)
    #LM.pwr <- exp(-smry$deviance/N)
    
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
  
  #cf <- apply(cf,1,applyTemplate,template=coef.template)
  
  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=cf,sumstat=sumstat,contrasts=obj$contrasts,xlevels=smry$xlevels,call=obj$call)
}

getSummary.simex <- function(obj,
            alpha=.05,
            ...){

  smry <- summary(obj)
  modsmry <- summary(obj$model)
  N <- if(length(weights(obj$model))) sum(weights(obj$model))
    else {
        if(length(modsmry$df) > 1) sum(modsmry$df[1:2])
        else obj$model$nobs
    }

  ## Asymptotics not guaranteed to be here.
  coef <- smry$coef$jackknife

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  sumstat <- c(N=N,
               LR=NA,
               df=NA,
               p=NA,
               Aldrich.Nelson=NA,
               McFadden=NA,
               Cox.Snell=NA,
               Nagelkerke=NA,
               logLik=NA,
               deviance=NA,
               AIC=NA,
               BIC=NA)

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=smry$xlevels,call=obj$call)
}

