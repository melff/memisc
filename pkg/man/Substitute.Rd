\name{Substitute}
\alias{Substitute}
\title{Substitutions in Language Objects}
\description{
  \code{Substitute} differs from \code{\link{substitute}}
  in so far as its first argument can be a variable that
  contains an object of mode "language". In that case,
  substitutions take place inside this object.
}
\usage{
Substitute(lang,with)
}
\arguments{
  \item{lang}{any object, unevaluated expression, or
    unevaluated language construct, such as a sequence
    of calls inside braces
    }
  \item{with}{
    a named list, environment, data frame or data set.
  }
}
\details{
  The function body is just
  \code{do.call("substitute",list(lang,with))}.
}
\value{
  An object of storage mode "language" or "symbol".
}
\examples{
lang <- quote(sin(x)+z)
substitute(lang,list(x=1,z=2))
Substitute(lang,list(x=1,z=2))
}
\keyword{programming}