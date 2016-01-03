getSummary.glm <- function(obj,
                           alpha=.05,
                           ...){
  
  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj),na.rm=TRUE)
  else sum(smry$df[1:2])
  
  coef <- smry$coef
  
  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])
  
  coef <- cbind(coef,lower,upper)
  
  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  phi <- smry$dispersion
  LR <- smry$null.deviance - smry$deviance
  df <- smry$df.null - smry$df.residual
  
  ll <- logLik(obj)
  deviance <- deviance(obj)
  
  
  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(-smry$null.deviance/N)
    
    Aldrich.Nelson <- LR/(LR+N)
    McFadden <- 1- smry$deviance/smry$null.deviance
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
    phi         = phi,
    LR             = LR,
    df         = df,
    p             = p,
    logLik        = ll,
    deviance      = deviance,
    Aldrich.Nelson = Aldrich.Nelson,
    McFadden      = McFadden,
    Cox.Snell       = Cox.Snell,
    Nagelkerke    = Nagelkerke,
    AIC           = AIC,
    BIC           = BIC,
    N             = N
  )
  
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=obj$xlevels,call=obj$call)
}