\name{measurement}
\alias{measurement}
\alias{measurement,ANY-method}
\alias{measurement,item-method}
\alias{measurement,data.set-method}
\alias{measurement<-}
\alias{measurement<-,item-method}
\alias{measurement<-,data.set-method}
\alias{set_measurement}
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
\S4method{measurement}{data.set}(x)
\S4method{measurement}{data.set}(x) <- value
is.nominal(x)
is.ordinal(x)
is.interval(x)
is.ratio(x)
set_measurement(x,\dots)
}
\arguments{
  \item{x}{an object, usually of class \code{"item"}.}
  \item{value}{
    for the \code{item} method, 
    a character string; either "nominal", "ordinal", "interval", or
    "ratio";
    for the \code{data.set} method,
    a list of character vectors with variable names,
    where the names of the list corresponds to a measurement level and
    and the list elements indicates the variables to which the
    measurement levels are assigned.
  }
  \item{\dots}{vectors of variable names, either symbols or character
    strings, tagged with the intended measurement level.
  }
}
\value{
  The \code{item} method of \code{measurement(x)} returns a character
  string, the \code{data.set} method returns a named character vector,
  where the name of each element is a variable name and each  
  
  \code{is.nominal}, \code{is.ordinal}, \code{is.interval}, \code{is.ratio}
  return a logical value.
}
\references{
Stevens, Stanley S. 1946. "On the theory of scales of measurement." \emph{Science} 103: 677-680.
}
\seealso{\code{\link{data.set}}, \code{\link{item}}}
\examples{
vote <- sample(c(1,2,3,8,9),size=30,replace=TRUE)
labels(vote) <- c(Conservatives         =  1,
                  Labour                =  2,
                  "Liberal Democrats"   =  3,
                  "Don't know"          =  8,
                  "Answer refused"      =  9
                  )
missing.values(vote) <- c(8,9)
as.data.frame(vote)[[1]]
measurement(vote) <- "interval"
as.data.frame(vote)[[1]]
group <- sample(c(1,2),size=30,replace=TRUE)
labels(group) <- c(A=1,B=2)
DataS <- data.set(group,vote)
measurement(DataS)
measurement(DataS) <- list(interval=c("group","vote"))
head(as.data.frame(DataS))
DataS <- set_measurement(DataS,
                         nominal=c(group,vote))
head(as.data.frame(DataS))
}
\keyword{manip}
