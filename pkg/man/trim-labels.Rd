\name{trim_labels}
\alias{trim_labels}
\alias{trim_labels,item.vector-method}
\alias{trim_labels,data.set-method}
\title{Trim Codes from the Labels of an Item}
\description{
  Occasionally, labels of codes in a survey data sets (e.g. from the
  2016 American National Election Study) include a
  character representation of the codes being labelled. While there may
  be technical reasons for this, it is often inconvenient (e.g. if one
  wants to reorder the labelled codes). The function \code{trim_labels}
  trims the code representations (if they are present.) 
}
\usage{
trim_labels(x,\dots)
\S4method{trim_labels}{item.vector}(x,\dots)
\S4method{trim_labels}{data.set}(x,\dots)
}
\arguments{
  \item{x}{An object -- an "item" object or a "data.set" object}
  \item{\dots}{Further arguments, currently ignored}
}
\details{
  The "data.set" method applies the "item.vector" method to all the
  labelled items in the data set.

  The "item.vector" returns a copy of its argument with modified labels,
  where a label such as "1. First alternative" is changed into "First alternative".
}
\examples{
x <- as.item(sample(1:3,10,replace=TRUE),
             labels=c("1. One"=1,
                      "2. Two"=2,
                      "2. Three"=3))
y <- as.item(sample(1:2,10,replace=TRUE),
             labels=c("1. First category"=1,
                      "2. Second category"=2))
ds <- data.set(x,y)
x <- trim_labels(x)
codebook(x)
ds <- trim_labels(ds)
codebook(ds)
}
