setSummaryTemplate(lmerMod = c("Log-likelihood" = "($logLik:f#)",
                     "Deviance" = "($deviance:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     "N" = "($N:d)"))

setSummaryTemplate(glmerMod = c("Log-likelihood" = "($logLik:f#)",
                               "Deviance" = "($deviance:f#)",
                               "AIC" = "($AIC:f#)",
                               "BIC" = "($BIC:f#)",
                               "N" = "($N:d)"))

getSummary.merMod <- function (obj, alpha = 0.05, varPar.as.coef=TRUE, ...) {

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
  varcor <- smry$varcor

  VarPar <- matrix(nrow=0,ncol=ncol(coef))
  VarPar.rnames <- c()
  
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
    vp.i <- matrix(NA,nrow=length(vr.i)+length(cv.i),ncol=ncol(VarPar))
    vp.i[,1] <- c(vr.i,cv.i)
    VarPar <- rbind(VarPar,vp.i)
    VarPar.rnames <- c(VarPar.rnames, vrnames.i,cvnames.i)
  }
  if(smry$sigma>1){
    vp.i <- matrix(NA,nrow=1,ncol=ncol(VarPar))
    vp.i[1] <- smry$sigma^2
    VarPar <- rbind(VarPar,vp.i)
    VarPar.rnames <- c(VarPar.rnames,"Var(residual)")
  }
  
  colnames(coef) <- colnames(VarPar) <- c("est", "se", "stat", "p", "lwr", "upr")
  rownames(VarPar) <- format(VarPar.rnames,justify="right")
  
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
  
  N <- nobs(obj)
  G <- smry$ngrps
  names(G) <- paste("Groups",names(G),sep=" - ")

  sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC,
               BIC = BIC, N = N)
  ## Return model summary.
  
  if(varPar.as.coef)
    coef <- rbind(coef,VarPar)
  
  list(coef= coef,
       sumstat = sumstat, extra.stats= G,
       contrasts = Contr, ## Reuse 'Contr'
       xlevels = xlevels, call = obj@call)
}
