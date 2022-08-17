\name{coarsen}
\alias{coarsen}
\alias{coarsen.numeric}
\title{Coarsen a vector into a factor with a lower number of levels}
\description{
  \code{coarsen} can be used to obtain a factor from a vector, similar
  to \code{\link[base]{cut}}, but with less technical and more "aesthetic"
  labels of the factor levels.
}
\usage{
coarsen(x,\dots)
\method{coarsen}{numeric}(x,
        n=5,
        pretty=TRUE,
        quantiles=!pretty,
        breaks=NULL,
        brackets=FALSE,
        sep=if(brackets)";"else if(quantiles) "-" else " - ",
        left="[",
        right="]",
        range=FALSE,
        labels=NULL,
        \dots)
}
\arguments{
  \item{x}{a vector, usually a numeric vector}
  \item{n}{number of categories of the resulting factor}
  \item{pretty}{a logical value, whether \code{\link[base]{pretty}}
    should be used to compute the breaks.}
  \item{quantiles}{a logical value, whether \code{\link[stats]{quantile}}
    should be used to compute the breaks.}
  \item{breaks}{a vector of break points or \code{NULL}.}
  \item{brackets}{a logical value, whether the labels should include brackets.}
  \item{sep}{a character string, used as a separator between upper and
    lower boundaries in the labels.}
  \item{left}{a character string, to be used as the left bracket}
  \item{right}{a character string, to be used as the right bracket}
  \item{range}{a logical value, whether the minimum and maximum of
    \code{x} should be included into \code{breaks}.}
  \item{labels}{an optional character vector of labels.}
  \item{\dots}{further arguments, passed on to \code{\link[base]{pretty}}
    or \code{\link[stats]{quantile}} if applicable.
  }
}


\examples{
x <- rnorm(200)

table(coarsen(x))

table(coarsen(x,quantiles=TRUE))

table(coarsen(x,brackets=TRUE))

table(coarsen(x,breaks=c(-1,0,1)))

table(coarsen(x,breaks=c(-1,0,1),
              range=TRUE,labels=letters[1:4]))

}
\keyword{manip}
