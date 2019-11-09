\name{labels}
\alias{labels}
\alias{labels,item-method}
\alias{labels<-}
\alias{labels<-,item,ANY-method}
\alias{labels<-,ANY,NULL-method}
\alias{labels<-,item,NULL-method}
\alias{labels<-,vector,ANY-method}
\alias{value.labels-class}
\alias{initialize,value.labels-method}
\alias{Arith,value.labels,ANY-method}
\alias{[,value.labels,numeric,missing,missing-method}
\alias{[,value.labels,logical,missing,missing-method}
\alias{coerce,numeric,value.labels-method}
\alias{coerce,character,value.labels-method}
\alias{coerce,value.labels,numeric-method}
\alias{coerce,value.labels,character-method}
\alias{as.vector,value.labels-method}
\alias{show,value.labels-method}
\title{Value Labels}
\description{
Value labels associate character labels to possible values
of an encoded survey item. Value labels are represented
as objects of class "value.labels".

Value labels of an item can be obtained
using \code{labels(x)} and
can be associated to items and to vectors
using labels(x) <- value

Value labels also can be updated using the  \code{+}
and \code{-} operators.
}
\usage{
labels(object,\dots)
labels(x) <- value
}
\arguments{
  \item{object}{any object.}
  \item{\dots}{further arguments for other methods.}
  \item{x}{a vector or "item" object.}
  \item{value}{an object of class "value.labels" or
    a vector that can be coerced into an "value.labels" object or NULL}
}
\examples{
  x <- as.item(rep(1:5,4),
      labels=c(
          "First"      = 1,
          "Second"     = 2,
          "Third"      = 3,
          "Fourth"     = 4,
          "Don't know" = 5
        ),
      missing.values=5,
      annotation = c(
        description="test"
      ))
  labels(x)
  labels(x) <- labels(x) - c("Second"=2)
  labels(x)
  labels(x) <- labels(x) + c("Second"=2)
  labels(x)

  puvl <- getOption("print.use.value.labels")
  options(print.use.value.labels=FALSE)
  x
  options(print.use.value.labels=TRUE)
  x
  options(print.use.value.labels=puvl)
}
