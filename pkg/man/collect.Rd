\name{collect}
\alias{collect}
\alias{collect.default}
\alias{collect.array}
\alias{collect.matrix}
\alias{collect.table}
\alias{collect.data.frame}
\alias{collect.data.set}
\title{Collect Objects}
\description{
  \code{collect} gathers several objects into one, matching the
  elements or subsets of the objects by \code{\link{names}} or \code{\link{dimnames}}.
}
\usage{
collect(\dots,names=NULL,inclusive=TRUE)
\method{collect}{default}(\dots,names=NULL,inclusive=TRUE)
\method{collect}{array}(\dots,names=NULL,inclusive=TRUE)
\method{collect}{matrix}(\dots,names=NULL,inclusive=TRUE)
\method{collect}{table}(\dots,names=NULL,sourcename="arg",fill=0)
\method{collect}{data.frame}(\dots,names=NULL,inclusive=TRUE,
                                  fussy=FALSE,warn=TRUE,sourcename="arg")
\method{collect}{data.set}(\dots,names=NULL,inclusive=TRUE,
                                  fussy=FALSE,warn=TRUE,sourcename="arg")
}
\arguments{
  \item{\dots}{more atomic vectors, arrays, matrices, tables, data.frames or data.sets}
  \item{names}{optional character vector; in case of the default and array methods,
    giving \code{\link{dimnames}} for the new dimension that identifies the
    collected objects; in case of the data.frame and data.set methods,
    levels of a factor indentifying the collected objects.
    }
  \item{inclusive}{logical, defaults to TRUE; should unmatched elements included? See details below.}
  \item{fussy}{logical, defaults to FALSE; should it count as an error, if variables with same
    names of collected data.frames/data.sets have different attributes?}
  \item{warn}{logical, defaults to TRUE; should an warning be given, if variables with same
    names of collected data.frames/data.sets have different attributes?}
  \item{sourcename}{name of the factor that identifies the collected data.frames or data.sets}
  \item{fill}{numeric; with what to fill empty table cells, defaults to zero, assuming
    the table contains counts}
}
\value{
  If \code{x} and all following \dots arguments are vectors of the same mode (numeric,character, or logical)
  the result is a matrix with as many columns as vectors. If argument \code{inclusive} is TRUE,
  then the number of rows equals the number of names that appear at least once in each of the
  vector names and the matrix is filled with \code{NA} where necessary,
  otherwise the number of rows equals the number of names that are present in \emph{all}
  vector names.

  If \code{x} and all \dots arguments are matrices or arrays of the same mode (numeric,character, or logical)
  and \eqn{n} dimension the result will be a \eqn{n+1} dimensional array or table. The extend of the
  \eqn{n+1}th dimension equals the number of matrix, array or table arguments,
  the extends of the lower dimension depends on the \code{inclusive} argument:
  either they equal to the number of dimnames that appear at least once for each given
  dimension and the array is filled with \code{NA} where necessary,
  or they equal to the number of dimnames that appear in all arguments
  for each given dimension.

  If \code{x} and all \dots arguments are tables then the result will be a table.
  The result

  If \code{x} and all \dots arguments are data frames or data sets, the
  result is a data frame or data set.
  The number of variables of the resulting data frame or data set depends on
  the \code{inclusive} argument. If it is true, the number of variables
  equals the number of variables that appear in each of the arguments at least once
  and variables are filled with \code{NA} where necessary, otherwise the
  number of variables equals the number of variables that are present in
  all arguments.
}
\examples{
x <- c(a=1,b=2)
y <- c(a=10,c=30)

x
y

collect(x,y)
collect(x,y,inclusive=FALSE)

X <- matrix(1,nrow=2,ncol=2,dimnames=list(letters[1:2],LETTERS[1:2]))
Y <- matrix(2,nrow=3,ncol=2,dimnames=list(letters[1:3],LETTERS[1:2]))
Z <- matrix(3,nrow=2,ncol=3,dimnames=list(letters[1:2],LETTERS[1:3]))

X
Y
Z

collect(X,Y,Z)
collect(X,Y,Z,inclusive=FALSE)

X <- matrix(1,nrow=2,ncol=2,dimnames=list(a=letters[1:2],b=LETTERS[1:2]))
Y <- matrix(2,nrow=3,ncol=2,dimnames=list(a=letters[1:3],c=LETTERS[1:2]))
Z <- matrix(3,nrow=2,ncol=3,dimnames=list(a=letters[1:2],c=LETTERS[1:3]))

collect(X,Y,Z)
collect(X,Y,Z,inclusive=FALSE)

df1 <- data.frame(a=rep(1,5),b=rep(1,5))
df2 <- data.frame(a=rep(2,5),b=rep(2,5),c=rep(2,5))
collect(df1,df2)
collect(df1,df2,inclusive=FALSE)

data(UCBAdmissions)
Male <- as.table(UCBAdmissions[,1,])
Female <- as.table(UCBAdmissions[,2,])
collect(Male,Female,sourcename="Gender")
collect(unclass(Male),unclass(Female))

Male1 <- as.table(UCBAdmissions[,1,-1])
Female2 <- as.table(UCBAdmissions[,2,-2])
Female3 <- as.table(UCBAdmissions[,2,-3])
collect(Male=Male1,Female=Female2,sourcename="Gender")
collect(Male=Male1,Female=Female3,sourcename="Gender")
collect(Male=Male1,Female=Female3,sourcename="Gender",fill=NA)

f1 <- gl(3,5,labels=letters[1:3])
f2 <- gl(3,6,labels=letters[1:3])
collect(f1=table(f1),f2=table(f2))

}
\keyword{utilities}
\keyword{manip}
