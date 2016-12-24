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
  \item{vcov}{a function that returns a variance matrix estimate, a
    given matrix that is such an estimate, or a character string that
    identifies a function that returns a variance matrix estimate
    (e.g. \code{"HAC"} for \code{vcovHAC}).
  }
  \item{\dots}{further arguments, passed to \code{vcov()} or, respectively,
    to the parent method of \code{summary()}  }
}
\details{
Using \code{withVCov()} an alternative variance-covariance matrix is
attributed to a fitted model object. Such a matrix may be produced by
any of the variance estimators provided by the "sandwich" package or
any package that extends it.

\code{withVCov()} has no consequences on how a fitted model itself is
printed or represented, but it does have consequences what standard
errors are reported, when the function \code{summary()} or the function
\code{mtable()} is applied.

\code{withSE()} is a convenience front-end to \code{withVCov()}. It can
be called in the same way as \code{withVCov}, but also allows to specify
the type of variance estimate by a character string that identifies
the function that gives the covariance matrix (e.g. \code{"OPG"} for
\code{vcovOPG}).
}
\value{
  \code{withVCov} returns a slightly modified model object: It adds an
  attribute named ".VCov" that contains the alternate covaraince matrix
  and modifies the class attribute. If e.g. the original model object has class
  "lm" then the model object modified by \code{withVCov} has the class
  attribute \code{c("withVCov.lm", "withVCov", "lm")}.  
}
\examples{
## Generate poisson regression relationship
x <- sin(1:100)
y <- rpois(100, exp(1 + x))
## compute usual covariance matrix of coefficient estimates
fm <- glm(y ~ x, family = poisson)

library(sandwich)
fmo <- withVCov(fm,vcovOPG)
vcov(fm)
vcov(fmo)

summary(fm)
summary(fmo)

mtable(Default=fm,
       OPG=withSE(fm,"OPG"),
       summary.stats=c("Deviance","N")
       )

vo <- vcovOPG(fm)

mtable(Default=fm,
       OPG=withSE(fm,vo),
       summary.stats=c("Deviance","N")
       )
}