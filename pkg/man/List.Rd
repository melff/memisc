\name{List}
\alias{List}
\title{Create a list and conveniently supply names to its elements}
\description{\code{List} creates a list and names its elements after the
  arguments given, in a manner analogously to \code{\link{data.frame}}}
\usage{
  List(\dots)
}
\arguments{
  \item{\dots}{tagged or untagged arguments from which the list is
    formed. If the untagged arguments are variables from the englosing
  environment, their names become the names of the list elements.}
}
\examples{
  num <- 1:3
  strng <- c("a","b","A","B")
  logi <- rep(FALSE,7)
  List(num,strng,logi)
}