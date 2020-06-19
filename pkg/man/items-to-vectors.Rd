\name{items-to-vectors}
\alias{as.data.frame.character.item}
\alias{as.data.frame.double.item}
\alias{as.data.frame.integer.item}
\alias{as.data.frame.datetime.item}
\alias{as.data.frame.Date.item}
\alias{as.vector,item-method}
\alias{as.numeric,item-method}
\alias{as.integer,item-method}
\alias{as.factor,item.vector-method}
\alias{as.ordered,item.vector-method}
\alias{as.character,item.vector-method}
\alias{as.character,datetime.item-method}
\alias{as.character,Date.item-method}

\title{How Survey Items Are Converted into "Ordinary" Data Vectors}
\description{
    Survey item objects in are numeric or character vectors with some extra information
    that may helpful for for managing and documenting survey data, but they are not suitable
    for statistical data analysis. To run regressions etc. one should convert 
    \code{\link{item}} objects into "ordinary" numeric vectors or factors.
    This means that codes or values declared as "missing" (if present) are translated into
    the generial missing value \code{NA}, while value labels (if defined) are translated into
    factor levels.
  }
\usage{
# The following methods can be used to covert items into
# vectors with a given mode or into factors. 
\S4method{as.vector}{item}(x, mode = "any")
\S4method{as.numeric}{item}(x, \dots)
\S4method{as.integer}{item}(x, \dots)
\S4method{as.factor}{item.vector}(x)
\S4method{as.ordered}{item.vector}(x)
\S4method{as.character}{item.vector}(x, use.labels = TRUE, include.missings = FALSE, \dots)
\S4method{as.character}{datetime.item.vector}()
\S4method{as.character}{Date.item.vector}()
# The following methods are unlikely to be useful in practice, other than
# that they are called internally by the 'as.data.frame()' method for "data.set"
# objects.
\method{as.data.frame}{character.item}(x, row.names = NULL, optional = FALSE, \dots)
\method{as.data.frame}{double.item}(x, row.names = NULL, optional = FALSE, \dots)
\method{as.data.frame}{integer.item}(x, row.names = NULL, optional = FALSE, \dots)
\method{as.data.frame}{Date.item}(x, row.names = NULL, optional = FALSE, \dots)
\method{as.data.frame}{datetime.item}(x, row.names = NULL, optional = FALSE, \dots)
}

\arguments{
  \item{x}{an object in class "item","item.vector", etc., as relevant
    for the respective conversion method.}
  \item{mode}{the mode of the vector to be returned, usually \code{"numeric"},
    \code{"integer"}, or \code{"charcater"}}
  \item{use.labels}{logical,should value labels be used for creating
    the character vector?}
  \item{include.missings}{logical; if \code{TRUE}, declared missing values are
    not converted into \code{NA}, but into character strings with \code{"*"} as the "missingness marker"
    added at the beginning.}
  \item{row.names}{optional row names, see \code{\link[base]{as.data.frame}}}
  \item{optional}{a logical value, see \code{\link[base]{as.data.frame}}}
  \item{\dots}{other arguments, ignored.}
}

\value{
The function \code{as.vector()} returns a logical, numeric, or character
depending on the \code{mode=} argument. If \code{mode="any"}, the vector
has the mode that corresponds to the (internal) mode of the item
vector, that is, an item in class "integer.item" will become an integer
vector, an item in class "double.item" will become a double-precision
numeric vector, an item in class "character.item" will become a
character vector; since the internal mode of a "dateitem.item" or a
"Date.item" vector is numeric, a numeric vector will be returned.

The functions \code{as.integer()}, \code{as.numeric()}, \code{as.character()}, 
\code{as.factor()}, and \code{as.ordered()} return an integer, numeric,
or character vector, or an ordered or unordered factor, respectively.

When \code{as.data.frame()} is applied to an survey item object, the
result is a single-column data frame, where the single column is a
numeric vector or character vector or factor depending on the
\code{\link{measurement}} attribute of the item. In particular, if the
\code{\link{measurement}} attribute equals \code{"ratio"} or
\code{"interval"} this column will be the result of \code{as.vector()},
if the \code{\link{measurement}} attribute equals \code{"ordinal"} this
column will be an ordered factor (see \code{\link{ordered}}), and if
the \code{\link{measurement}} attribute equals \code{"nominal"} this
column will be an unordered factor (see \code{\link{factor}}).

All these functions have in common that values declared as "missing" by
virtue of the \code{\link{value.filter}} attribute will be turned into \code{NA}. 
}

\seealso{
  \code{\link{items}}
  \code{\link{annotation}}
  \code{\link{labels}}
  \code{\link{value.filter}}
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
  str(x)
  summary(x)
  as.numeric(x)

  test <- as.item(rep(1:6,2),labels=structure(1:6,
                                      names=letters[1:6]))

  as.factor(test)
  as.numeric(test)
  as.character(test)
  as.character(test,include.missings=TRUE)

  as.data.frame(test)[[1]]
}
\keyword{manip}
