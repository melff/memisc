\name{Reshape}
\alias{Reshape}
\title{Reshape data frames or data sets}
\description{\code{Reshape} is a convenience
  wrapper around \code{\link[stats]{reshape}} with a somewhat simpler
  syntax.}
\usage{
Reshape(data,spec,direction)
}
\arguments{
  \item{data}{a data frame or data set to be reshaped.}
  \item{spec}{a list of vectors of variable names that represent
    repeated measurements and a vector
    of values that indentify measurement occasions.}
  \item{direction}{a character string, should be either equal "long"
    or "wide".}
}
\examples{
example.data.wide <- data.frame(
    v  = c(35,42),
    x1 = c(1.1,2.1),
    x2 = c(1.2,2.2),
    x3 = c(1.3,2.3),
    x4 = c(1.4,2.4),
    y1 = c(2.5,3.5),
    y2 = c(2.7,3.7),
    y3 = c(2.9,3.9))
example.data.wide

example.data.long <- Reshape(data=example.data.wide,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 # N.B. it is possible to
                                 # specify 'empty' i.e. missing
                                 # measurements
                                 y=c(y1,y2,y3,),
                                 t=1:4
                             ),
                             direction="long")
example.data.long

example.data.wide <- Reshape(data=example.data.long,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,),
                                 t=1:4
                             ),
                             direction="wide")
example.data.wide

example.data.long <- Reshape(data=example.data.wide,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,)
                             ),
                             direction="long")

example.data.long

example.data.wide <- Reshape(data=example.data.long,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,)
                             ),
                             direction="wide")
example.data.wide
}