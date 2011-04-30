\name{relabel}
\alias{relabel}
\alias{relabel.default}
\alias{relabel.factor}
\alias{relabel4}
\alias{relabel4,item-method}
\title{Change labels of factors or labelled objects}
\description{
  Function \code{relabel} changes the labels of a factor or any object
  that has a \code{names}, \code{labels}, \code{value.labels}, or \code{variable.labels} attribute.
  Function \code{relabel4} is an (internal) generic which is called by \code{relabel}
  to handle S4 objects.
}
\usage{
\method{relabel}{default}(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
\method{relabel}{factor}(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)


\S4method{relabel4}{item}(x, \dots)
# This is an internal method, see details.
# Use relabel(x, \dots) for 'item' objects
}
\arguments{
  \item{x}{An object with a \code{names}, \code{labels}, \code{value.labels}, or \code{variable.labels} attribute}
  \item{\dots}{A sequence of named arguments, all of type character}
  \item{gsub}{a logical value; if TRUE, \code{\link{gsub}} is used to change the
      labels of the object. That is, instead of substituting whole labels, substrings of the
      labels of the object can changed.}
  \item{fixed}{a logical value, passed to \code{\link{gsub}}. If TRUE,
    substitutions are by fixed strings and not by regular expressions.}
  \item{warn}{a logical value; if TRUE, a warning is issues if a
      a change of labels was unsuccessful.}
  }
\details{
  This function changes the names or labels of \code{x} according to the
  remaining arguments.
  If \code{gsub} is FALSE, argument tags are the \emph{old}
  labels, the values are the new labels.
  If \code{gsub} is TRUE, arguments are substrings of the labels
  that are substituted by the argument values.

  Function \code{relabel} is S3 generic. If its first argument is an S4 object,
  it calls the (internal) \code{relabel4} generic function.
}
\value{
  The object \code{x} with new labels defined by the \dots arguments.
}
\examples{
  f <- as.factor(rep(letters[1:4],5))
  levels(f)
  F <- relabel(f,
    a="A",
    b="B",
    c="C",
    d="D"
    )
  levels(F)
  
  f <- as.item(f)
  labels(f)
  F <- relabel(f,
    a="A",
    b="B",
    c="C",
    d="D"
    )
  labels(F)
}


\keyword{manip}
