\name{withSE}
\alias{withSE}
\alias{withVCov}
\alias{withVCov.lm}
\alias{vcov.withVCov}
\alias{summary.withVCov}
\alias{summary.withVCov.lm}
\title{Add Alternative Variance Estimates to Models Estimates}
\description{
  A simple object-orientation infrastructure to add alternative standard
  errors, e.g. sandwich estimates or New-West standard errors to 
  fitted regression-type models, such as fitted by \code{lm()} or \code{glm()}.  
}
\usage{

withSE(object, vcov, \dots) 

withVCov(object, vcov, \dots)

\method{withVCov}{lm}(object, vcov, \dots)

\method{summary}{withVCov}(object, \dots)
\method{summary}{withVCov.lm}(object, \dots)
}
\arguments{
  \item{object}{a fitted model object}
  \item{vcov}{a function that returns a variance matrix estimate, or a
    given matrix that is such an estimate}
  \item{\dots}{further arguments, passed to \code{vcov()} or, respectively,
    to the parent method of \code{summary()}  }
}
\details{
Using \code{withVCov()} an alternative variance-covariance matrix is
attributed to a fitted model object. Such a matrix may be produced by
any of the variance estimators provided by the "sandwich" package or
any package that extends it.

\code{withSE()} is merely an alias of \code{withVCov()} that better
represents what the wants to eventually obtain (wile the original name
of the function reflects what it actually does).

\code{withVCov()} has no consequences on how a fitted model itself is
printed or represented, but it does have consequences what standard
errors are reported, when the function \code{summary()} or the function
\code{mtable()} is applied.
}
\value{
  \code{withVCov} returns a slightly modified model object: It adds an
  attribute named ".VCov" that contains the alternate covaraince matrix
  and modifies the class attribute. If e.g. the original model object has class
  "lm" then the model object modified by \code{withVCov} has the class
  attribute \code{c("withVCov.lm", "withVCov", "lm")}.  
}
\examples{
## generate poisson regression relationship
x <- sin(1:100)
y <- rpois(100, exp(1 + x))
## compute usual covariance matrix of coefficient estimates
fm <- glm(y ~ x, family = poisson)

library(sandwich)
fmv <- withVCov(fm,vcovOPG)
vcov(fm)
vcov(fmv)

summary(fm)
summary(fmv)

mtable(Default=fm,
       HAC=withVCov(fm,vcovHAC),
       OPG=withVCov(fm,vcovOPG),
       summary.stats=c("Deviance","N")
       )
}