\name{trimws}
\alias{trimws}
\title{Trim Leading and Trailing Whitespaces}
\description{
  \code{trimws} returns a character string with leading and/or trailing
  whitespaces removed. This is just a shorthand for an appropriate call to \link{sub}.
}
\usage{
  trimws(x,left=TRUE,right=TRUE)
}
\arguments{
  \item{x}{a character vector}
  \item{left}{logical, if \code{TRUE} leading whitespace is removed}
  \item{right}{logical, if \code{TRUE} trailing whitespace is removed}
}

\value{
  The character vector \code{x} with whitespace removed.
}

\examples{
  teststr <- c(" Hello World!   ","I am here.  ")
  trimws(teststr)
  trimws(teststr,left=FALSE)
  trimws(teststr,right=FALSE)
}
\keyword{utilities}
