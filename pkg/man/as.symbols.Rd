\name{as.symbols}
\alias{as.symbols}
\alias{syms}
\title{Construction of Lists of Symbols}
\description{
  \code{as.symbols} and \code{syms} are functions potentially useful
  in connection with \code{foreach} and \code{xapply}.
  \code{as.symbols} produces a list of symbols from a character vector,
  while \code{syms} returns a list of symbols from symbols given as arguments,
  but it can be used to construct patterns of symbols.
}
\usage{
as.symbols(x)
syms(\dots,paste=FALSE,sep="")
}
\arguments{
  \item{x}{a character vector}
  \item{\dots}{character strings or (unquoted) variable names}
  \item{paste}{logical value; should the character strings
    \code{\link{paste}}d into one string?}
  \item{sep}{a separator string, passed to \code{\link{paste}}. }
}
\value{
A list of language symbols (results of \code{\link{as.symbol}} - not graphical
symbols!).
}
\examples{
  as.symbols(letters[1:8])
  syms("a",1:3,paste=TRUE)

  sapply(syms("a",1:3,paste=TRUE),typeof)
}
\keyword{programming}