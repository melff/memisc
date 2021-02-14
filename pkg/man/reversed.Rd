\name{reversed}
\alias{reversed}
\alias{reversed,item.vector-method}
\alias{reversed,factor-method}
\title{Reverse the codes of a survey item or the levels of a factor}
\description{
The function \code{reversed()} returns a copy of its argument with codes
or levels in reverse order.
}
\usage{
reversed(x)
\S4method{reversed}{item.vector}(x)
\S4method{reversed}{factor}(x)
}
\arguments{
  \item{x}{An object -- an "item" object or a "data.set" object}
}
\value{
  If the argument of the function \code{reversed()} than either the
  unique valid values or the labelled valid values recoded into the
  reverse order.

  If th argument is a factor than the function returns the factor with
  levels in reverse order.
}
\examples{
ds <- data.set(
    x = as.item(sample(c(1:3,9),100,replace=TRUE),
                labels=c("One"=1,
                         "Two"=2,
                         "Three"=3,
                         "Missing"=9)))
df <- as.data.frame(ds)
ds <- within(ds,{
    xr <- reversed(x)
})
codebook(ds)
df <- within(df,{
    xr <- reversed(x)
})
codebook(df)
}
