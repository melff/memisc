setSummaryTemplate(lmerMod = c("Log-likelihood" = "($logLik:f#)",
                     "Deviance" = "($deviance:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     N     = "($N:d)"))

setSummaryTemplate(glmerMod = c("Log-likelihood" = "($logLik:f#)",
                               "Deviance" = "($deviance:f#)",
                               "AIC" = "($AIC:f#)",
                               "BIC" = "($BIC:f#)",
                               N     = "($N:d)"))

getSummary.merMod <- function (obj, alpha = 0.05, ...) {

  smry <- summary(obj)
  coef <- smry$coefficients
  
  lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,2])
  upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,2])
  if (ncol(coef) == 3) {
    p <- (1 - pnorm(abs(coef[, 3]))) * 2
    coef <- cbind(coef, p, lower, upper)
  }
  else {
    coef <- cbind(coef, lower, upper)
  }

  dn.cf <- list(
      rownames(coef),
      c("est","se","stat","p","lwr","upr"),
      names(obj@frame)[1]
  )
  dim(coef) <- c(dim(coef)[1],dim(coef)[2],1)
  dimnames(coef) <- dn.cf
    
  varcor <- smry$varcor

  VarPar <- NULL

  if(smry$sigma!=1){
    vp.i <- matrix(NA,nrow=1,ncol=6)
    vp.i[1] <- smry$sigma^2
    dim(vp.i) <- c(dim(vp.i),1)
    dimnames(vp.i) <- list("Var(residual)",
                           c("est","se","stat","p","lwr","upr"),
                           names(obj@frame)[1])
    VarPar <- list(residual=vp.i)
  }

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
                           names(obj@frame)[1])
    VarPar <- c(VarPar,structure(list(vp.i),names=lv.i))
  }
    
  ## Factor levels.
  xlevels <- list()
  Contr <- names(attr(model.matrix(obj), "contrasts"))
  for (c in Contr) xlevels[[c]] <- levels(obj@frame[,c])

  ## Model fit statistics.
  ll <- logLik(obj)[1]
  isREML <- inherits(obj@resp,"lmerResp") && obj@resp$REML > 0
  if(!isREML)
    deviance <- deviance(obj)
  else 
    deviance <- lme4::REMLcrit(obj)
  AIC <- AIC(obj)
  BIC <- BIC(obj)
  
  G <-as.integer(smry$ngrps)
  names(G) <- names(smry$ngrps)
    
  sumstat <- c(logLik = ll,
               deviance = deviance,
               AIC = AIC,
               BIC = BIC,
               N=nobs(obj))
  ## Return model summary.
  
  ans <- list(coef= coef)

  ans <- c(ans,VarPar)
    
  ans <- c(ans,       
           list(Groups = G,
                sumstat = sumstat,
                contrasts = Contr, ## Reuse 'Contr'
                xlevels = xlevels,
                call = obj@call))
  return(ans)
}
