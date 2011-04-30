\name{sample-methods}
\alias{sample.data.frame}
\alias{sample.data.set}
\title{Take a Sample from a Data Frame-like Object}
\description{
  The methods below are convenience short-cuts to
  take samples from data frames and data sets.
  They result in a data frame or data set, respectively,
  the rows of which are a sample of the complete
  data frame/data set.
}
\usage{
\method{sample}{data.frame}(x, size, replace = FALSE, prob = NULL, \dots)
\method{sample}{data.set}(x, size, replace = FALSE, prob = NULL, \dots)
}
\arguments{
  \item{x}{a data frame or data set.}
  \item{size}{an (optional) numerical value, the sample size,
    defaults to the total number of rows of \code{x}.}
  \item{replace}{a logical value, determines whether
    sampling takes place with or without replacement.}
  \item{prob}{a vector of sampling probabities or NULL.}
  \item{\dots}{further arguments, ignored.}
}
\value{A data frame or data set.}