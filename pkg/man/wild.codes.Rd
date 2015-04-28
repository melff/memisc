\name{wild.codes}
\alias{wild.codes}
\alias{wild.codes,item-method}
\title{
 Table of frequencies for unlabelled codes
}
\description{
  The function \code{wild.codes} creates a table of frequencies of those codes
  of an item that do not have labelled attached to them. This way, it helps to identify
  coding errors.
}
\usage{
wild.codes(x)
\S4method{wild.codes}{item}(x)
}
\arguments{
  \item{x}{an object of class "item"}
}
\value{
  A table of frequencies (i.e. an array of class "table")
}
