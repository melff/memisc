\name{contr}
\alias{contr.treatment}
\alias{contr.sum}
\alias{contr}
\alias{contrasts}
\alias{contrasts,item-method}
\alias{contrasts,ANY-method}
\alias{contrasts<-}
\alias{contrasts<-,item-method}
\alias{contrasts<-,ANY-method}
\title{Convenience Methods for Setting Contrasts}
\description{
  This package provides modified versions of
  \code{\link[stats]{contr.treatment}} and
  \code{\link[stats]{contr.sum}}. \code{contr.sum}
  gains an optional \code{base} argument, analog to the
  one of \code{contr.treatment}, furthermore,
  the \code{base} argument may be the name of a
  factor level.

  \code{contr} returns a function that calls either
  \code{contr.treatment}, \code{contr.sum}, etc.,
  according to the value given to its first argument.

  The \code{contrasts} method for \code{"item"} objects
  returns a contrast matrix or a function to produce
  a contrast matrix for the factor into which
  the item would be coerced via \code{as.factor} or \code{as.ordered}.
  This matrix or function can be specified by
  using \code{contrasts(x)<-value}
}
\usage{
contr(type,\dots)
contr.treatment(n, base=1,contrasts=TRUE)
contr.sum(n,base=NULL,contrasts=TRUE)
\S4method{contrasts}{item}(x,contrasts=TRUE,\dots)
\S4method{contrasts}{item}(x,how.many) <- value
# These methods are defined implicitely by making 'contrasts' generic.
\S4method{contrasts}{ANY}(x,contrasts=TRUE,\dots)
\S4method{contrasts}{ANY}(x,how.many) <- value
}
\arguments{
  \item{type}{a character vector, specifying the type of the contrasts.
    This argument should have a value such that, if e.g. \code{type="something"},
    then there is a function \code{contr.something} that produces
    a contrast matrix.
    }
  \item{\dots}{further arguments, passed to \code{contr.treatment}, etc. }
  \item{n}{a number of factor levels or a vector of factor levels names, see e.g. \code{\link[stats]{contr.treatment}}.}
  \item{base}{a number of a factor level or the names of a factor level,
    which specifies the baseline category,
    see e.g. \code{\link[stats]{contr.treatment}} or NULL.
    }
  \item{contrasts}{a logical value, see  \code{\link[stats]{contrasts}}}
  \item{how.many}{the number of contrasts to generate, see \code{\link[stats]{contrasts}}}
  \item{x}{a factor or an object of class "item"}
  \item{value}{a matrix, a function or the name of a function}
}
\value{
\code{contr} returns a funtion that calls one of \code{contr.treatment},
\code{contr.sum,\dots}.
\code{contr.treatment} and \code{contr.sum} return contrast matrices.
\code{contrasts(x)} returns the "contrasts" attribute of an
object, which may be a function name, a function, a contrast matrix or NULL.
}
\examples{
ctr.t <- contr("treatment",base="c")
ctr.t
ctr.s <- contr("sum",base="c")
ctr.h <- contr("helmert")
ctr.t(letters[1:7])
ctr.s(letters[1:7])
ctr.h(letters[1:7])

x <- factor(rep(letters[1:5],3))
contrasts(x)
x <- as.item(x)
contrasts(x)
contrasts(x) <- contr.sum(letters[1:5],base="c")
contrasts(x)
missing.values(x) <- 5
contrasts(x)
contrasts(as.factor(x))

# Obviously setting missing values after specifying
# contrast matrix breaks the contrasts.
# Using the 'contr' function, however, prevents this:

missing.values(x) <- NULL
contrasts(x) <- contr("sum",base="c")
contrasts(x)
missing.values(x) <- 5
contrasts(x)
contrasts(as.factor(x))
}