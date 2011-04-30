\name{Descriptives}
\alias{Descriptives}
\alias{Descriptives,atomic-method}
\alias{Descriptives,item.vector-method}
\title{Vectors of Univariate Sample Statistics}
\description{
  \code{Descriptives(x)} gives a vector of sample statistics
  for use in \code{\link{codebook}}.
}
\usage{
Descriptives(x,...)
\S4method{Descriptives}{atomic}(x,...)
\S4method{Descriptives}{item.vector}(x,...)
}
\arguments{
  \item{x}{an atomic vector or \code{"item.vector"} object. }
  \item{\dots}{further arguments, to be passed to future methods.}
}
\value{
A numeric vector of sample statistics, containing the range, the
mean, the standard deviation, the skewness and the (excess) kurtosis.
}
\examples{
x <- rnorm(100)
Descriptives(x)
}