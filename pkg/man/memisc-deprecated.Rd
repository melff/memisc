\name{memisc-deprecated}
\alias{memisc-deprecated}
\alias{fapply}
\alias{fapply.default}
\title{Deprecated Functions in Package \pkg{memisc}}
\description{
  These functions are provided for compatibility with older versions of
  \pkg{memisc} only, and may be defunct as soon as the next release.
}
\usage{
fapply(formula,data,\dots) # calls UseMethod("fapply",data)
\method{fapply}{default}(formula, data, subset=NULL,
      names=NULL, addFreq=TRUE,\dots)
}
\arguments{
  \item{formula}{a formula. The right hand side includes one or more
  grouping variables separated by '+'. These may be factors, numeric,
  or character vectors. The left hand side may be empty,
  a numerical variable, a factor, or an expression.
  See details below.}
  \item{data}{an environment or data frame or an object coercable into a data frame.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{names}{an optional character vector giving names to the
    result(s) yielded by the expression on the left hand side of \code{formula}.
    This argument may be redundant if the left hand side results in is a named vector.
    (See the example below.)}
  \item{addFreq}{a logical value. If TRUE and
      \code{data} is a table or a data frame with a variable
      named "Freq", a call to
      \code{table}, \code{\link{Table}}, \code{\link{percent}}, or \code{\link{nvalid}}
      is supplied by an additional argument \code{Freq}
      and a call to \code{table} is translated into
      a call to \code{Table}.
    }
  \item{\dots}{further arguments, passed to methods or ignored.}
}

