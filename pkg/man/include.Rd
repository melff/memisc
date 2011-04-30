\name{include}
\alias{include}
\alias{uninclude}
\alias{detach.sources}
\title{Attach a Source File to the Search Path}
\description{
  Functions \code{include} and \code{uninclude} provide a simple mechanism
  for modularisation without the need to code a full-blown package.
  Functions in an \code{include}d sourcefile are attached to the search path,
  but are not visible in the global environment. If an experienced user
  has written quite a number of utility functions for her/his daily work,
  but does not want to have listed them in each \code{ls()} call,
  s/he can just \code{include} a source file containing these utility
  functions. \code{uninclude} just reverts the attachment of a source file
  into the search path. \code{detach.sources} detaches all
  source files from the search path.
}
\usage{
include(filename,warn=FALSE)
uninclude(filename)
detach.sources()
}
\arguments{
  \item{filename}{character string, path to a file containing R code.}
  \item{warn}{logical value; should a warning issued if the file
    has already been \code{include}d? }
}
\examples{
\dontrun{
include("~/R/my-functions.R")
include("~/R/more-of-my-functions.R")

ls() # The functions defined in these files will not show up here

# ... but here
search()
ls("source:~/R/my-functions.R")
ls("source:~/R/more-of-my-functions.R")

z <- one.of.my.functions(100) # ... and are available.

uninclude("~/R/my-functions.R") 

detach.sources()  
}
}
\keyword{programming}