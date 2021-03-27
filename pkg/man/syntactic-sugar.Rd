\name{syntactic-sugar}
\alias{\%#\%}
\alias{\%##\%}
\alias{\%@\%}
\alias{\%@\%<-}
\title{Syntactic Sugar for Setting Annotations and Attributes}
\description{The operator \code{\%#\%} can be used to attach a
  \code{\link{description}} annotation to an object. \code{\%##\%} can be
  used to attach a character vector of annotations to an object.
  \code{\%@\%} returns the attribute with the name given as second
  argument. With \code{\%@\%} it is also possible to assign attributes.
}
\usage{
  x \%#\% descr
  x \%##\% annot
  x \%@\% nm
  x \%@\% nm <- value
}
\arguments{
  \item{x}{an object, usually and \code{\link{item}} or a vector.}
  \item{descr}{a character string}
  \item{annot}{a named character vector; its contents are added to the
    "annotation" attribute of \code{x}. Existing elements are kept.}
  \item{nm}{a character string, the name of the attribute being set or
    requested.}
  \item{value}{any kind of object that can be attached as an attribute.}
}
\examples{
test1 <- 1 \%#\% "One"
# This is equivalent to:
# test <- 1
# description(test) <- "One"
description(test1)
# Results in "One"

# Not that it makes sense, but ...
test2 <- 2 \%##\% c(
                    Precedessor = 0,
                    Successor   = 2
                 )
# This is equivalent to:
# test2 <- 2
# annotation(test2) <- c(
#                    Precedessor = 0,
#                    Successor   = 2
#                 )
annotation(test2)

# The following examples are equivalent to
# attr(test2,"annotation")
test2 \%@\% annotation

test2 \%@\% "annotation"

test2 \%@\% another.attribute <- 42
# This is equivalent to attr(test2,"another.attribute") <- 42

attributes(test2)
}
