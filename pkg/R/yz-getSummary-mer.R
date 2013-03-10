setSummaryTemplate(mer = c("Log-likelihood" = "($logLik:f#)",
                     "Deviance" = "($deviance:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     "N" = "($N:d)"))

getSummary.mer <- function (obj, alpha = 0.05, varPar.as.coef=TRUE, ...) {
  ## BUGFIX by M.E. 2012-07-24
  ## For whatever reason a simple call to summary does not work...

  smry <- getMethod("summary","mer")(obj)
  coef <- smry@coefs
  
  lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,2])
  upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,2])
  if (ncol(smry@coefs) == 3) {
    ## BUGFIX by M.E.: should be abs(.) here
    p <- (1 - pnorm(abs(smry@coefs[, 3]))) * 2
    coef <- cbind(coef, p, lower, upper)
  }
  else {
    coef <- cbind(coef, lower, upper)
  }
  RE <- smry@REmat

  ## BUGFIX by M.E. 2012-07-25: Standard deviations are not standard errors!
  VarPar <- cbind(as.numeric(RE[,3]), NA, NA,NA,NA,NA)
  
  for(ii in 1:nrow(RE)){
    if(!nzchar(RE[ii,1])) RE[ii,1] <- RE[ii-1,1]
  }
  rownames(VarPar) <- paste0("Var(",RE[,2],"|",RE[,1],")")
  
  
  colnames(coef) <- colnames(VarPar) <- c("est", "se", "stat", "p", "lwr", "upr")

  
  ## Factor levels.
  xlevels <- list()
  Contr <- names(attr(getMethod("model.matrix","mer")(obj), "contrasts"))
  for (c in Contr) xlevels[[c]] <- levels(obj@frame[,c])

  ## Model fit statistics.
  ll <- getMethod("logLik","mer")(obj)[1]
  deviance <- getMethod("deviance","mer")(obj)
  AIC <- AIC(obj)
  BIC <- BIC(obj)
  N <- as.numeric(smry@dims["n"])
  G <- smry@ngrps
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