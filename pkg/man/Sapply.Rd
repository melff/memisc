\name{Sapply}
\alias{Lapply}
\alias{Sapply}
\title{A Dimension Preserving Variant of "sapply" and "lapply"}
\description{
  \code{Sapply} is equivalent to \code{\link{sapply}}, except
  that it preserves the dimension and dimension names of the
  argument \code{X}. It also preserves the dimension of
  results of the function \code{FUN}.
  It is intended for application to results e.g.
  of a call to \code{\link{by}}. \code{Lapply} is an analog
  to \code{lapply} insofar as it does not try to simplify
  the resulting \code{list} of results of \code{FUN}.
}
\usage{
Sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
Lapply(X, FUN, ...)
  }
\arguments{
  \item{X}{a vector or list appropriate to a call to \code{sapply}.}
  \item{FUN}{a function.}
  \item{...}{optional arguments to \code{FUN}.}
  \item{simplify}{a logical value; should the result be simplified to a vector or matrix if possible?}
  \item{USE.NAMES}{logical; if \code{TRUE} and if \code{X} is character, use \code{X} as names for the result unless it had names already.}
}

\value{
  If \code{FUN} returns a scalar, then the result has the same dimension
  as \code{X}, otherwise the dimension of the result is enhanced relative
  to \code{X}.
}

\examples{
berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
berktest1 <- By(~Dept+Gender,
                glm(cbind(Admitted,Rejected)~1,family="binomial"),
                data=berkeley)
berktest2 <- By(~Dept,
                glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                data=berkeley)

sapply(berktest1,coef)
Sapply(berktest1,coef)

sapply(berktest1,function(x)drop(coef(summary(x))))
Sapply(berktest1,function(x)drop(coef(summary(x))))

sapply(berktest2,coef)
Sapply(berktest2,coef)
sapply(berktest2,function(x)coef(summary(x)))
Sapply(berktest2,function(x)coef(summary(x)))
}
\keyword{utilities}
\keyword{misc}
