\name{query}
\alias{query}
\alias{query,data.set-method}
\alias{query,importer-method}
\alias{query,item-method}

\title{Query an Object for Information}

\description{
  The function \code{query} can be used to search an object
  for a keyword.

  The \code{\link{data.set}} and \code{\link{importer}} methods perform such a search
  through the annotations and value labels of
  the items in the data set.
  }
\usage{
query(x,pattern,\dots)
\S4method{query}{data.set}(x,pattern,\dots)
\S4method{query}{importer}(x,pattern,\dots)

\S4method{query}{item}(x,pattern,\dots)
# (Called by the methods above.)
}
\arguments{
  \item{x}{an object}
  \item{pattern}{a character string that gives the pattern to be searched
    for}
  \item{\dots}{optional arguments such as
    \describe{
      \item{\code{fuzzy}}{logical, TRUE by default; use fuzzy search via \code{\link{agrep}} or regexp search
         via \code{\link{grep}} }
      \item{\code{extended}}{logical, defaults to FALSE; passed to \code{\link{grep}} }
      \item{\code{perl}}{logical, defaults to TRUE; passed to \code{\link{grep}} }
      \item{\code{fixed}}{logical, defaults to TRUE; passed to \code{\link{grep}} }
      \item{\code{ignore.case}}{logical, defaults to TRUE; passed to \code{\link{grep}} or \code{\link{agrep}}}
      \item{\code{insertions}}{numerical value, defaults
                    to 0.999999999; passed to \code{\link{agrep}} }
      \item{\code{deletions}}{numerical value, defaults to 0; passed to \code{\link{agrep}} }
      \item{\code{substitutions}}{numerical value, defaults to 0; passed to \code{\link{agrep}} }
    }
  }
}
\value{
  If both the annotation and the value labels of an item match the pattern
  the \code{query} method for 'item' objects returns a list containing the annotation
  and the value labels, otherwise if only the annotation or the value labels
  match the pattern, either the annotation or the value labels are returned,
  otherwise if neither matches the pattern, \code{query} returns \code{NULL}.

  The methods of \code{query} for 'data.set' and 'importer' objects return
  a list of all non-\code{NULL} query results of all items contained by these
  objects, or \code{NULL}.
}
\examples{
nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR",
                     package="memisc")
nes1948 <- spss.portable.file(nes1948.por)
query(nes1948,"TRUMAN")
}

\keyword{manip}
