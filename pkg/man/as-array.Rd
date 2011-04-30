\name{as.array}
\alias{as.array}
\alias{as.array,data.frame-method}
\title{Converting Data Frames into Arrays}
\description{
  The \code{as.array} for data frames
  takes all factors in a data frame and uses them
  to define the dimensions of the resulting array,
  and fills the array with the values of
  the remaining numeric variables.

  Currently, the data frame must contain all
  combinations of factor levels.
}
\usage{
\S4method{as.array}{data.frame}(x,data.name=NULL,\dots)
}
\arguments{
  \item{x}{a data frame}
  \item{data.name}{
    a character string, giving the name
    attached to the dimension
    that corresponds to the
    numerical variables in the data frame
    (that is, the \code{name} attached to
    the corresponding element of the
    \code{dimnames} list).
    }
  \item{\dots}{other arguments, ignored.}
}
\value{
  An array
}
\examples{
BerkeleyAdmissions <- to.data.frame(UCBAdmissions)
BerkeleyAdmissions
as.array(BerkeleyAdmissions,data.name="Admit")
try(as.array(BerkeleyAdmissions[-1,],data.name="Admit"))
}