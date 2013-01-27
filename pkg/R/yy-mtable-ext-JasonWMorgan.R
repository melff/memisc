## ----------------------------------------------------------------------------
## Author: Jason W. Morgan
## URL: http://leftcensored.skepsi.net
## Date: 2011.06.21
##
## Description: This file contains functions for extracting model summaries
##   according to the requirements of mtable() and toLatex() available in the
##   memisc packages.
##
## Note: This code is based heavily on the examples and functions provided by
##   Martin Elff in his memisc package. If you find it useful, you should cite
##   *his* work. You can find his website here: http://www.martin-elff.net
##
## Copyright 2011 Jason W. Morgan
## 
## This program is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the Free
## Software Foundation, either version 3 of the License, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
## more details.
##
## You should have received a copy of the GNU General Public License along with
## this program.  If not, see <http://www.gnu.org/licenses/>.
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Supported model types
## ----------------------------------------------------------------------------
##
## coxph, survreg - Cox proportional hazards models and parametric survival
##   models from the survival library.
##
## aftreg, phreg, weibreg - parametric AFT, proportional hazards, and Weibull
##    models available from the eha library.
##
## mer - multilevel mixed effects models as provided by the lme4 package. The
##    model objects produced in the beta lme4a package are not supported.
##
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Summary functions for survival objects.
## ----------------------------------------------------------------------------

setSummaryTemplate(coxph = c("Log-likelihood" = "($logLik:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     "N" = "($N:d)"))

setSummaryTemplate(survreg = c("Log-likelihood" = "($logLik:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     "N" = "($N:d)"))

setSummaryTemplate(aftreg = c("Log-likelihood" = "($logLik:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     "N" = "($N:d)"))

getSummary.coxph <- function(obj, alpha = 0.05, ...) {
    N <- obj$n

    ## Take the info right out of the object to guarantee that I get the right
    ## standard errors.
    b <- coef(obj)
    se <- sqrt(diag(vcov(obj)))
    coef <- cbind(b, se, b / se, 1-pnorm(abs(b/se)))
    lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,2])
    upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,2])
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    ll <- obj$loglik[2]
    K <- length(coef[,1])
    AIC <- -2*ll + 2*K
    BIC <- -2*ll + K*log(N) 
    sumstat <- c(logLik = ll, AIC = AIC, BIC = BIC, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}

getSummary.survreg <- function(obj, alpha = 0.05, ...) {
    coef <- cbind(c(coef(obj), obj$scale), sqrt(diag(obj$var)))
    N <- obj$df.residual + length(coef)
    stat  <- coef[, 1] / coef[, 2]
    pval  <- 1-pnorm(abs(stat))
    lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,2])
    upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,2])
    coef  <- cbind(coef, stat, pval, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    rownames(coef)[nrow(coef)] <- "scale"
    ll <- obj$loglik[2]
    K <- length(coef[,1])
    AIC <- -2*ll + 2*K
    BIC <- -2*ll + K*log(N) 
    sumstat <- c(logLik = ll, AIC = AIC, BIC = BIC, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}

## aftreg(), phreg(), weibreg() are part of the eha package.
getSummary.aftreg <- function(obj, alpha = 0.05, ...) {
    N <- obj$n
    coef  <- cbind(coef(obj), sqrt(diag(obj$var)))
    stat  <- coef[, 1] / coef[, 2]
    pval  <- 1-pnorm(abs(stat))
    lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[, 2])
    upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[, 2])
    coef  <- cbind(coef, stat, pval, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    ll <- obj$loglik[2]
    K <- length(coef[,1])
    AIC <- -2*ll + 2*K
    BIC <- -2*ll + K*log(N) 
    sumstat <- c(logLik = ll, AIC = AIC, BIC = BIC, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
         xlevels = obj$xlevels, call = obj$call)
}

getSummary.phreg <- function(obj, alpha = 0.05, ...) {
  getSummary.aftreg(obj, alpha = alpha, ...)
}

getSummary.weibreg <- function(obj, alpha = 0.05, ...) {
  getSummary.aftreg(obj, alpha = alpha, ...)
}



