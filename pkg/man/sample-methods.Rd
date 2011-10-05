\name{sample-methods}
\alias{sample,data.frame-method}
\alias{sample,data.set-method}
\alias{sample,importer-method}
\title{Take a Sample from a Data Frame-like Object}
\description{
  The methods below are convenience short-cuts to
  take samples from data frames and data sets.
  They result in a data frame or data set, respectively,
  the rows of which are a sample of the complete
  data frame/data set.
}
\usage{
\S4method{sample}{data.frame}(x, size, replace = FALSE, prob = NULL)
\S4method{sample}{data.set}(x, size, replace = FALSE, prob = NULL)
\S4method{sample}{importer}(x, size, replace = FALSE, prob = NULL)
}
\arguments{
  \item{x}{a data frame or data set.}
  \item{size}{an (optional) numerical value, the sample size,
    defaults to the total number of rows of \code{x}.}
  \item{replace}{a logical value, determines whether
    sampling takes place with or without replacement.}
  \item{prob}{a vector of sampling probabities or NULL.}
}
\value{A data frame or data set.}

\examples{
for(.i in 1:4)
  print(sample(iris,5))
}