\name{reorder.array}
\alias{reorder}
\alias{reorder.array}
\alias{reorder.matrix}
\title{Reorder an Array or Matrix}
\description{
  \code{reorder.array} reorders an array along a specified
  dimension according given names, indices or results of
  a function applied.
}
\usage{
\method{reorder}{array}(x,dim=1,names=NULL,indices=NULL,FUN=mean,...)
\method{reorder}{matrix}(x,dim=1,names=NULL,indices=NULL,FUN=mean,...)
}
\arguments{
  \item{x}{An array}
  \item{dim}{An integer specifying the dimension along which \code{x} should be ordered.}
  \item{names}{A character vector}
  \item{indices}{A numeric vector}
  \item{FUN}{A function that can be used in \code{apply(x,dim,FUN)} }
  \item{...}{further arguments, ignored.}
}
\details{
  Typical usages are
  \preformatted{
  reorder(x,dim,names)
  reorder(x,dim,indices)
  reorder(x,dim,FUN)
  }

  The result of \code{rename(x,dim,names)} is \code{x}
  reordered such that \code{dimnames(x)[[dim]]} is equal to
  the concatenation of those elements of \code{names}
  that are in \code{dimnames(x)[[dim]]} and the remaining elements
  of \code{dimnames(x)[[dim]]}.

  The result of \code{rename(x,dim,indices)} is \code{x}
  reordered along \code{dim} according to \code{indices}.

  The result of \code{rename(x,dim,FUN)} is \code{x}
  reordered along \code{dim} according to \code{order(apply(x,dim,FUN))}.
  
}

\value{
  The reordered object \code{x}.
}
\seealso{
  The default method of \code{\link[stats]{reorder}} in package \code{stats}.
}
\examples{
  (M <- matrix(rnorm(n=25),5,5,dimnames=list(LETTERS[1:5],letters[1:5])))
  reorder(M,dim=1,names=c("E","A"))
  reorder(M,dim=2,indices=3:1)
  reorder(M,dim=1)
  reorder(M,dim=2)
}
\keyword{manip}
