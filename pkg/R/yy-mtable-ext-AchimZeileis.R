## AER::ivreg

getSummary.ivreg <- function(obj, alpha = 0.05, ...) {
  ## extract summary object
  s <- summary(obj)
  
  ## coefficient matrix and confidence interval
  cf <- cbind(s$coefficients, confint(obj, level = 1 - alpha))
  colnames(cf) <- c("est", "se", "stat", "p", "lwr", "upr")

  ## further summary statistics
  sstat <- c("sigma" = s$sigma,
    "r.squared" = s$r.squared, "adj.r.squared" = s$adj.r.squared,
    "Wald" = s$waldtest[1], "numdf" = s$waldtest[3], "dendf" = s$waldtest[4],
    "p" = s$waldtest[2], "N" = nobs(obj))

  ## return everything
  return(list(
    coef = cf,
    sumstat = sstat,
    contrasts = obj$contrasts,
    xlevels = obj$xlevels,
    call = obj$call
  ))
}

setSummaryTemplate("ivreg" = c(
  "R-squared" = "($r.squared:f#)",
  "adj. R-squared" = "($adj.r.squared:f#)",
  "sigma" = "($sigma:#)",
  "Wald" = "($Wald:f#)",
  "p" = "($p:f#)",
  "N" = "($N:d)"
))

## AER::tobit

getSummary.tobit <- function(obj, alpha = 0.05, ...) {
  ## extract summary object
  s <- summary(obj)
  
  ## coefficient matrix and confidence interval
  ## compute confidence interval manually to include Log(scale)
  cf <- cbind(s$coefficients,
    s$coefficients[, 1] + qnorm(alpha/2) * s$coefficients[, 2],
    s$coefficients[, 1] + qnorm(1 - alpha/2) * s$coefficients[, 2])
  colnames(cf) <- c("est", "se", "stat", "p", "lwr", "upr")

  ## Improvement by ME: deal with log-scale parameter
  sp.row <- match("Log(scale)",rownames(cf))
  sp <- cf[sp.row,,drop=FALSE]
  cf <- cf[-sp.row,,drop=FALSE]
  
  ## further summary statistics
  sstat <- c(
    "scale" = s$scale,
    "Wald" = s$wald,
    "numdf" = sum(s$df) - s$idf,
    "p" = pchisq(s$wald, sum(s$df) - s$idf, lower.tail = FALSE),
    "N" = nobs(obj),
    "logLik" = as.vector(logLik(obj)),
    "AIC" = AIC(obj),
    "BIC" = BIC(obj))

  ## return everything
  return(list(
    estimates = list(coef=cf,scale=sp),
    sumstat = sstat,
    contrasts = obj$contrasts,
    xlevels = obj$xlevels,
    call = obj$call
  ))
}

setSummaryTemplate("tobit" = c(
  "Scale" = "($scale:#)",
  "Wald" = "($Wald:f#)",
  "p" = "($p:f#)",
  "Log-likelihood" = "($logLik:f#)",
  "AIC" = "($AIC:f#)",
  "BIC" = "($BIC:f#)",
  "N" = "($N:d)"
))

## pscl:::hurdle, pscl:::zeroinfl

getSummary.hurdle <- getSummary.zeroinfl <- function(obj, alpha = 0.05, ...) {
  ## extract coefficient summary
  cf <- summary(obj)$coefficients
  ## augment with confidence intervals
  cval <- qnorm(1 - alpha/2)
  for(i in seq_along(cf)) cf[[i]] <- cbind(cf[[i]],
    cf[[i]][, 1] - cval * cf[[i]][, 2],
    cf[[i]][, 1] + cval * cf[[i]][, 2])
  ## collect in array
  nam <- unique(unlist(lapply(cf, rownames)))
  acf <- array(dim = c(length(nam), 6, length(cf)),
    dimnames = list(nam, c("est", "se", "stat", "p", "lwr", "upr"), names(cf)))
  for(i in seq_along(cf)) acf[rownames(cf[[i]]), , i] <- cf[[i]]
  
  ## return everything
  return(list(
    coef = acf,
    sumstat = c(
      "N" = obj$n,
      "logLik" = as.vector(logLik(obj)),
      "AIC" = AIC(obj),
      "BIC" = AIC(obj, k = log(obj$n))
    ),
    contrasts = obj$contrasts,
    xlevels = obj$xlevels,
    call = obj$call
  ))
}

setSummaryTemplate("hurdle" = c(
  "Log-likelihood" = "($logLik:f#)",
  "AIC" = "($AIC:f#)",
  "BIC" = "($BIC:f#)",
  "N" = "($N:d)"
))

setSummaryTemplate("zeroinfl" = c(
  "Log-likelihood" = "($logLik:f#)",
  "AIC" = "($AIC:f#)",
  "BIC" = "($BIC:f#)",
  "N" = "($N:d)"
))

## betareg::betareg

getSummary.betareg <- function(obj, alpha = 0.05, ...) {
  ## extract coefficient summary
  s <- summary(obj)
  cf <- s$coefficients
  ## augment with confidence intervals
  cval <- qnorm(1 - alpha/2)
  for(i in seq_along(cf)) cf[[i]] <- cbind(cf[[i]],
    cf[[i]][, 1] - cval * cf[[i]][, 2],
    cf[[i]][, 1] + cval * cf[[i]][, 2])
  ## collect in array
  nam <- unique(unlist(lapply(cf, rownames)))
  acf <- array(dim = c(length(nam), 6, length(cf)),
    dimnames = list(nam, c("est", "se", "stat", "p", "lwr", "upr"), names(cf)))
  for(i in seq_along(cf)) acf[rownames(cf[[i]]), , i] <- cf[[i]]
  
  ## return everything
  return(list(
    coef = acf,
    sumstat = c(
      "N" = obj$n,
      "pseudo.r.squared" = s$pseudo.r.squared,
      "logLik" = as.vector(logLik(obj)),
      "AIC" = AIC(obj),
      "BIC" = AIC(obj, k = log(obj$n))
    ),
    contrasts = obj$contrasts,
    xlevels = obj$xlevels,
    call = obj$call
  ))
}

setSummaryTemplate("betareg" = c(
  "Pseudo R-sq." = "($pseudo.r.squared:f#)",
  "Log-likelihood" = "($logLik:f#)",
  "AIC" = "($AIC:f#)",
  "BIC" = "($BIC:f#)",
  "N" = "($N:d)"
))

getSummary.multinom <- function(obj, alpha = 0.05, ...) {
  ## extract coefficient summary
  s <- summary(obj)
  cf <- s$coefficients

  ## set up array
  acf <- array(dim = c(NCOL(cf), 6, NROW(cf)),
    dimnames = list(colnames(cf), c("est", "se", "stat", "p", "lwr", "upr"), rownames(cf)))
    
  ## coefficients and standard errors
  acf[, 1, ] <- t(cf)
  acf[, 2, ] <- t(s$standard.errors)

  ## compute z-statistic and asymptotic p-value
  acf[, 3, ] <- acf[, 1, ] / acf[, 2, ]
  acf[, 4, ] <- 2 * pnorm(abs(acf[, 3, ]), lower.tail = FALSE)
  
  ## asymptotic confidence intervals
  cval <- qnorm(1 - alpha/2)
  acf[, 5, ] <- acf[, 1, ] - cval * acf[, 2, ]
  acf[, 6, ] <- acf[, 1, ] + cval * acf[, 2, ]

  ## compute number of observations as sum of weights
  nobs <- sum(obj$weights)

  ## return everything
  return(list(
    coef = acf,
    sumstat = c(
      "N" = nobs,
      "deviance" = obj$deviance,
      "logLik" = as.vector(logLik(obj)),
      "AIC" = AIC(obj),
      "BIC" = AIC(obj, k = log(nobs))
    ),
    contrasts = obj$contrasts,
    xlevels = obj$xlevels,
    call = obj$call
  ))
}

setSummaryTemplate("multinom" = c(
  "Deviance" = "($deviance:f#)",
  "Log-likelihood" = "($logLik:f#)",
  "AIC" = "($AIC:f#)",
  "BIC" = "($BIC:f#)",
  "N" = "($N:d)"
))


if(FALSE) {

## packages
library("memisc")
library("AER")
library("pscl")
library("betareg")
library("nnet")

## ivreg
data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
m1_ivreg <- ivreg(log(packs) ~ log(rprice) | tdiff, data = CigarettesSW, subset = year == "1995")
m2_ivreg <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
  data = CigarettesSW, subset = year == "1995")
mtable(m1_ivreg, m2_ivreg) 

## tobit
data("Affairs", package = "AER")
m1_tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
  data = Affairs)
m2_tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
  right = 4, data = Affairs)
mtable(m1_tobit, m2_tobit)

## hurdle
data("bioChemists", package = "pscl")
m1_hurdle <- hurdle(art ~ . | kid5 + ment, data = bioChemists, dist = "negbin")
m2_hurdle <- hurdle(art ~ . | .,           data = bioChemists, dist = "negbin")
mtable(m1_hurdle, m2_hurdle)

## zeroinfl
m1_zeroinfl <- zeroinfl(art ~ . | kid5 + ment, data = bioChemists, dist = "negbin")
m2_zeroinfl <- zeroinfl(art ~ . | .,           data = bioChemists, dist = "negbin")
mtable(m1_zeroinfl, m2_zeroinfl)

## betareg
data("ReadingSkills", package = "betareg")
m1_betareg <- betareg(accuracy ~ dyslexia * iq | 1,             data = ReadingSkills)
m2_betareg <- betareg(accuracy ~ dyslexia * iq | dyslexia + iq, data = ReadingSkills)
mtable(m1_betareg, m2_betareg)

## multinom
data("BankWages", package = "AER")
m1_multinom <- multinom(job ~ education, data = BankWages,
  subset = gender == "male", trace = FALSE)
m2_multinom <- multinom(job ~ education + minority, data = BankWages,
  subset = gender == "male", trace = FALSE)
mtable(m1_multinom, m2_multinom)
}
