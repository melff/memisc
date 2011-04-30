\name{retain}
\alias{retain}
\title{Retain Objects in an Environment}
\description{\code{retain} removes all objects from the environment
  except those mentioned as argument.}
\usage{
retain(\dots, list = character(0), envir = parent.frame(),force=FALSE)
}
\arguments{
  \item{\dots}{names of objects to be retained, as names (unquoted)
    or character strings(quoted).}
  \item{list}{a character vector naming the objects to be retained.}
  \item{envir}{the environment from which the objects are removed
    that are not to be retained.}
  \item{force}{logical value. As a measure of caution, this
    function removes objects only from local environments,
    unless \code{force} equals TRUE. In that case, \code{retain} can
    also be used to clear the global environment, the user's workspace.}
}
\examples{
local({
  foreach(x=c(a,b,c,d,e,f,g,h),x<-1)
  cat("Objects before call to 'retain':\n")
  print(ls())
  retain(a)
  cat("Objects after call to 'retain':\n")
  print(ls())
})
x <- 1
y <- 2
retain(x)
}
\keyword{manip}