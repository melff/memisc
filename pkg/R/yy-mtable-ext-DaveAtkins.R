## The following two functions are contributed by
## Dave Atkins, PhD
## Research Associate Professor
## Center for the Study of Health and Risk Behaviors
## Department of  Psychiatry and Behavioral Science
## Unversity of Washington

getSummary_expcoef <- function(obj, alpha = 0.05, ...) UseMethod("getSummary_expcoef")

getSummary_expcoef.glm <- function (obj, alpha = 0.05, ...)
{
    smry <- summary(obj)
    N <- if (length(weights(obj)))
        sum(weights(obj))
    else sum(smry$df[1:2])
    coef <- cbind(exp(smry$coef[,1]), smry$coef[,2:4]) # NOTE: exponentiated coefficients
    lower <- exp(qnorm(p = alpha/2, mean = smry$coef[, 1], sd = coef[,
        2]))
    upper <- exp(qnorm(p = 1 - alpha/2, mean = smry$coef[, 1], sd = coef[,
        2]))
    # NOTE: exponentiated CI
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    phi <- smry$dispersion
    LR <- smry$null.deviance - smry$deviance
    df <- smry$df.null - smry$df.residual
    ll <- logLik(obj)
    deviance <- deviance(obj)
    if (df > 0) {
        p <- pchisq(LR, df, lower.tail = FALSE)
        L0.pwr <- exp(-smry$null.deviance/N)
        Aldrich.Nelson <- LR/(LR+N)
        McFadden <- 1 - smry$deviance/smry$null.deviance
        Cox.Snell <- 1 - exp(-LR/N)
        Nagelkerke <- Cox.Snell/(1 - L0.pwr)
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
    BIC <- AIC(obj, k = log(N))
    sumstat <- c(phi = phi, LR = LR, df = df, p = p, logLik = ll,
        deviance = deviance, McFadden = McFadden, Cox.Snell = Cox.Snell,
        Aldrich.Nelson = Aldrich.Nelson,
        Nagelkerke = Nagelkerke, AIC = AIC, BIC = BIC, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts,
        xlevels = obj$xlevels, call = obj$call)
}

if(FALSE){ ## Use Jason Morgan's version instead
  getSummary.lmer <- function (obj, alpha = 0.05, ...)
  {
      smry <- summary(obj)
      #N <- if (length(weights(obj))) ### NOTE: how to deal with groups/samp size?
      #    sum(weights(obj))
      #else sum(smry$df[1:2])
      coef <- smry@coefs
      lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,
          2])
      upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,
          2])
      if (ncol(smry@coefs) == 3) {
        ## BUGFIX: should be abs(.) here
        p <- (1 - pnorm(abs(smry@coefs[,3])))*2 # NOTE: no p-values for lmer() due to
                            # unclear dfs; calculate p-values based on z
        coef <- cbind(coef, p, lower, upper)
      } else {
          coef <- cbind(coef, lower, upper) # glmer will have 4 columns with p-values
          }
      colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
      #phi <- smry$dispersion
      #LR <- smry$null.deviance - smry$deviance
      #df <- smry$df.null - smry$df.residual
      ll <- smry@logLik[1]
      deviance <- smry@deviance[['ML']]
      #if (df > 0) {
      #    p <- pchisq(LR, df, lower.tail = FALSE)
      #    L0.pwr <- exp(-smry$null.deviance/N)
      #    McFadden <- 1 - smry$deviance/smry$null.deviance
      #    Cox.Snell <- 1 - exp(-LR/N)
      #    Nagelkerke <- Cox.Snell/(1 - L0.pwr)
      #}
      #else {
      #    LR <- NA
      #    df <- NA
      #    p <- NA
      #    McFadden <- NA
      #    Cox.Snell <- NA
      #    Nagelkerke <- NA
      #}
      AIC <- smry@AICtab[1][,1] # NOTE: these are both data.frames? not sure why...
      BIC <- smry@AICtab[2][,1]
      ### NOTE: don't see a similar slot for "xlevels" to get levels of
      ###   factor variables used as predictors; for time being, force
      ###   user to specify explicitly
      #if (fac != NULL) {
      #   n <- length(fac)
      # xlevels <- vector(n, mode = "list")
      # for (i in 1:n) {
      #   xlevels[i] <- levels(obj@frame[,fac[i]])
      #   }
      # }
      #sumstat <- c(phi = phi, LR = LR, df = df, p = p, logLik = ll,
      #    deviance = deviance, McFadden = McFadden, Cox.Snell = Cox.Snell,
      #    Nagelkerke = Nagelkerke, AIC = AIC, BIC = BIC, N = N)
      sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC, BIC = BIC,
                  N = obj@dims[['n']], phi=NA, LR=NA, df=NA, p=NA, McFadden=NA,
                  Cox.Snell=NA, Nagelkerke=NA, Aldrich.Nelson=NA)
      list(coef = coef, sumstat = sumstat,
        contrasts = attr(obj@X, "contrasts"),
          xlevels = NULL, call = obj@call)
  }

  ## lmer objects renamed to mer since 0.999375-16
  getSummary.mer <- getSummary.lmer
}
