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
                except=NULL,
                only=NULL,
                \dots)
}
\arguments{
  \item{x}{an object from class "item.vector" or "data.set".}
  \item{to}{a character vector, the target measurement level}
  \item{threshold}{the proportion of values, if reached the target
    measurement level is set}
  \item{except}{a vector with variable names, either as symbols
    (without quotation marks) or character strings (with quotation
    markes), the variables in the data set that are not to be
    changed by \code{measurement_autolevel()}.}
  \item{only}{a vector with variable names, either as symbols
    (without quotation marks) or character strings (with quotation
    markes), the variables in the data set that are to be
    changed by \code{measurement_autolevel()}.}
  \item{\dots}{other arguments, currently ignored.}
}
\examples{

 exvect <- as.item(rep(1:2,5))
 labels(exvect) <- c(a=1,b=2)
 codebook(exvect)
 codebook(measurement_autolevel(exvect))

 avect <- as.item(sample(1:3,16,replace=TRUE))
 labels(avect) <- c(a=1,b=2,c=3)
 bvect <- as.item(sample(1:4,16,replace=TRUE))
 labels(bvect) <- c(A=1,B=2,C=3,D=4)
 ds <- data.set(a=avect,b=bvect)
 codebook(ds)
 codebook(measurement_autolevel(ds))
 codebook(measurement_autolevel(ds,except=c(a,b)))
 codebook(measurement_autolevel(ds,only=a))
}
