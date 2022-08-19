\name{measurement_autolevel}
\alias{measurement_autolevel}
\alias{measurement_autolevel,ANY-method}
\alias{measurement_autolevel,item.vector-method}
\alias{measurement_autolevel,data.set-method}
\title{Automatically Adapt Measurement Levels}
\description{
  The generic function \code{measurement_autolevel} changes the measurement
  levels of "item" objects to "nominal" or "ordinal", if
  the proportion of its values that have labels is above a certain
  threshold.
}
\usage{
measurement_autolevel(x, \dots)
\S4method{measurement_autolevel}{ANY}(x, \dots) # Returns its argument as is
\S4method{measurement_autolevel}{item.vector}(x, 
                to=getOption("measurement.adapt.default","nominal"),
                threshold=getOption("measurement.adapt.threshold",.75),
                \dots)
\S4method{measurement_autolevel}{data.set}(x, 
                to=getOption("measurement.adapt.default","nominal"),
                threshold=getOption("measurement.adapt.threshold",.75),
                only=NULL,
                \dots)
}
\arguments{
  \item{x}{an object from class "item.vector" or "data.set".}
  \item{to}{a character vector, the target measurement level}
  \item{threshold}{the proportion of values, if reached the target
    measurement level is set}
  \item{only}{an optional character vector, the names of the
    variables of which the measurement levels should
    be automatically adapted.}
  \item{\dots}{other arguments, currently ignored.}
}
\examples{

 exvect <- as.item(rep(1:2,5))
 labels(exvect) <- c(a=1,b=2)
 codebook(exvect)
}
