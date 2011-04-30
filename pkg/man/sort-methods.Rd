\name{sort-methods}
\alias{sort.data.frame}
\alias{sort.data.set}
\title{Convenience Methods to Sort Data Frames and Data Sets}
\description{
The methods below return a sorted version of
the data frame or data set, given as first argument.
}
\usage{
\method{sort}{data.frame}(x,decreasing=FALSE,by=NULL,na.last=NA,\dots)
\method{sort}{data.set}(x,decreasing=FALSE,by=NULL,na.last=NA,\dots)
}
\arguments{
  \item{x}{a data frame or data set.}
  \item{decreasing}{a logical value, should sorting
    be in increasing or decreasing order?}
  \item{by}{
    a character name of variable names, by which to sort;
    a formula giving the variables, by which to sort;
    NULL, in which case, the data frame / data set
    is sorted by all of its variables.
    }
  \item{na.last}{for controlling the treatment of 'NA's. If 'TRUE', missing
          values in the data are put last; if 'FALSE', they are put
          first; if 'NA', they are removed}
  \item{\dots}{other arguments, currently ignored.}
}
\value{
A sorted copy of \code{x}.
}
\examples{
DF <- data.frame(
        a = sample(1:2,size=20,replace=TRUE),
        b = sample(1:4,size=20,replace=TRUE))
sort(DF)
sort(DF,by=~a+b)
sort(DF,by=~b+a)
sort(DF,by=c("b","a"))
sort(DF,by=c("a","b"))
}