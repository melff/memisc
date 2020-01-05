\name{dimrename}
\alias{dimrename}
\alias{colrename}
\alias{rowrename}
\title{Change dimnames, rownames, or colnames}
\description{
  These functions provide an easy way to change the \code{dimnames}, \code{rownames} or \code{colnames} of
  an array.
}
\usage{
dimrename(x, dim = 1, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
rowrename(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
colrename(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
}
\arguments{
  \item{x}{An array with dimnames}
  \item{dim}{A vector that indicates the dimensions}
  \item{\dots}{A sequence of named arguments}
  \item{gsub}{a logical value; if TRUE, \code{\link{gsub}} is used to change the
      \code{dimnames} of the object.
      That is, instead of substituting whole names, substrings of the
      \code{dimnames} of the object can changed.
      }
  \item{fixed}{a logical value, passed to \code{\link{gsub}}. If TRUE,
    substitutions are by fixed strings and not by regular expressions.}
  \item{warn}{logical; should a warning be issued if the pattern is not found?}
}
\details{
  \code{dimrename} changes the dimnames of \code{x} along dimension(s) \code{dim} according to the
  remaining arguments. The argument names are the \emph{old}
  names, the values are the new names.
  \code{rowrename} is a shorthand for changing the rownames,
  \code{colrename} is a shorthand for changing the colnames of a matrix
  or matrix-like object.

  If \code{gsub} is FALSE, argument tags are the \emph{old}
  \code{dimnames}, the values are the new \code{dimnames}.
  If \code{gsub} is TRUE, arguments are substrings of the \code{dimnames}
  that are substituted by the argument values.

}
\value{
  Object \code{x} with changed dimnames.
}

\examples{
m <- matrix(1,2,2)
rownames(m) <- letters[1:2]
colnames(m) <- LETTERS[1:2]
m
dimrename(m,1,a="first",b="second")
dimrename(m,1,A="first",B="second")
dimrename(m,2,"A"="first",B="second")

rowrename(m,a="first",b="second")
colrename(m,"A"="first",B="second")

# Since version 0.99.22 - the following also works:

dimrename(m,1,a=first,b=second)
dimrename(m,1,A=first,B=second)
dimrename(m,2,A=first,B=second)
}

\keyword{manip}
