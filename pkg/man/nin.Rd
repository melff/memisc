\name{negative match}
\alias{\%nin\%}
\title{Negative Match}
\description{
  \code{\%nin\%}
  is a convenience operator:
  \code{x \%nin\% table}
  is equivalent to
  \code{!(x \%in\% table).}
}
\usage{
  x \%nin\% table
}
\arguments{
  \item{x}{the values to be matched}
  \item{table}{a values to be match against}
}
\value{
  A logical vector
}
\examples{
x <- sample(1:6,12,replace=TRUE)
x \%in\% 1:3
x \%nin\% 1:3
}