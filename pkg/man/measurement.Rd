\name{measurement}
\alias{measurement}
\alias{measurement,ANY-method}
\alias{measurement,item-method}
\alias{measurement<-}
\alias{measurement<-,item-method}
\alias{is.nominal}
\alias{is.ordinal}
\alias{is.interval}
\alias{is.ratio}
\title{Levels of Measurement of Survey Items}
\description{
  The measurement level of a \code{"item"} object, which is one of "nominal", "ordinal", "interval", "ratio",
  determines what happens to it, if it or the \code{\link{data.set}}
  containing it is coerced into a \code{\link{data.frame}}.
  If the level of measurement level is "nominal", the it will be
  converted into an (unordered) \link{factor}, if the level of measurement is "ordinal",
  the item will be converted into an \link{ordered} vector. If the measurement
  is "interval" or "ratio", the item will be converted into a numerical vector.
}
\usage{
\S4method{measurement}{item}(x)
\S4method{measurement}{item}(x) <- value
is.nominal(x)
is.ordinal(x)
is.interval(x)
is.ratio(x)
}
\arguments{
  \item{x}{an object, usually of class \code{"item"}.}
  \item{value}{
    a character string; either "nominal", "ordinal", "interval", or "ratio".
    }
}
\value{
  \code{measurement(x)} returns a character string.
  \code{is.nominal}, \code{is.ordinal}, \code{is.interval}, \code{is.ratio}
  return a logical value.
}
\references{
Stevens, Stanley S. 1946. "On the theory of scales of measurement." \emph{Science} 103: 677-680.
}
\seealso{\code{\link{data.set}}, \code{\link{item}}}
\examples{
answer <- sample(c(1,2,3,8,9),size=30,replace=TRUE)
labels(answer) <- c(Conservatives         =  1,
                    Labour                =  2,
                    "Liberal Democrats"   =  3,
                    "Don't know"          =  8,
                    "Answer refused"      =  9
                    )
missing.values(answer) <- c(8,9)
as.data.frame(answer)[[1]]
measurement(answer) <- "interval"
as.data.frame(answer)[[1]]
}
\keyword{manip}