\name{Table}
\alias{Table}
\alias{Table,atomic-method}
\alias{Table,factor-method}
\alias{Table,item.vector-method}
\title{One-Dimensional Table of Frequences and/or Percentages}
\description{
  \code{Table} is a generic function that
  produces a table of counts or weighted counts
  and/or the corresponding percentages of an atomic vector,
  factor or \code{"item.vector"} object.
  This function is intended for use with
  \code{\link{Aggregate}} or \code{\link{genTable}}.
  The \code{"item.vector"} method is the workhorse
  of \code{\link{codebook}}.
  }
\usage{
\S4method{Table}{atomic}(x,weights=NULL,counts=TRUE,percentage=FALSE,\dots)
\S4method{Table}{factor}(x,weights=NULL,counts=TRUE,percentage=FALSE,\dots)
\S4method{Table}{item.vector}(x,weights=NULL,counts=TRUE,percentage=(style=="codebook"),
              style=c("table","codebook","nolabels"),
              include.missings=(style=="codebook"),
              missing.marker=if(style=="codebook") "M" else "*",\dots)
}
\arguments{
  \item{x}{an atomic vector, factor or \code{"item.vector"} object}
  \item{counts}{logical value, should the table contain counts?}
  \item{percentage}{logical value, should the table contain percentages?
  Either the \code{counts} or the \code{percentage} arguments or both
  should be \code{TRUE}. }
  \item{style}{character string, the style of the names or rownames of the table.}
  \item{weights}{a numeric vector of weights of the same length as \code{x}.}
  \item{include.missings}{a logical value; should missing values included into the table?}
  \item{missing.marker}{a character string, used to mark missing values
    in the table (row)names.}
  \item{\dots}{other, currently ignored arguments.}
}
\value{
  The atomic vector and factor methods return either a vector
  of counts or vector of percentages or a matrix of counts and percentages.
  The same applies to the \code{"item.vector"} vector method unless
  \code{include.missing=TRUE} and \code{percentage=TRUE},
  in which case total percentages and percentages of valid values
  are given.
}
\examples{
  with(as.data.frame(UCBAdmissions),Table(Admit,Freq))
  Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)

  A <- sample(c(1:5,9),size=100,replace=TRUE)
  labels(A) <- c(a=1,b=2,c=3,d=4,e=5,dk=9)
  missing.values(A) <- 9
  Table(A,percentage=TRUE)
}
\keyword{univar}
