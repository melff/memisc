\name{Mean}
\alias{Mean}
\alias{Median}
\alias{Min}
\alias{Max}
\alias{Weighted.Mean}
\alias{Var}
\alias{StdDev}
\alias{Cov}
\alias{Cor}
\alias{Range}
\title{Convenience wrappers for common statistical functions}
\description{\code{Mean()}, \code{Median()}, etc. are mere wrappers of
 the functions \code{mean()}, \code{median()}, etc. with the
 \code{na.rm=} optional argument set \code{TRUE} by default.}
\usage{
Mean(x, na.rm=TRUE, \dots)
Median(x, na.rm=TRUE, \dots)
Min(x, na.rm=TRUE, \dots)
Max(x, na.rm=TRUE, \dots)
Weighted.Mean(x, w, \dots, na.rm = TRUE)
Var(x, na.rm=TRUE, \dots)
StdDev(x, na.rm=TRUE, \dots)

Cov(x, y = NULL, use = "pairwise.complete.obs", \dots)
Cor(x, y = NULL, use = "pairwise.complete.obs", \dots)
Range(\dots, na.rm = TRUE, finite = FALSE)
}
\arguments{
  \item{x}{a (numeric) vector.}
  \item{y}{a (numeric) vector or \code{NULL}.}
  \item{w}{a (numeric) vector of weights.}
  \item{na.rm}{a logical value, see \code{\link[base]{mean}}.}
  \item{use}{a character string, see \code{\link[stats]{cor}}.}
  \item{\dots}{other arguments, passed to the wrapped functions.}
  \item{finite}{a logical value, see \code{\link[base]{range}}.}
}
