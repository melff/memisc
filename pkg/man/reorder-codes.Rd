\name{reverse_codes}
\alias{reverse_codes}
\alias{reverse_codes,item.vector-method}
\title{Reorder the Codes and Value Labels of a Survey Item}
\description{
The function \code{reorder_codes()} reorders the codes and carries over
the labels of a survey item object.

The function \code{reverse_codes()} changes the unique codes of an item
in such a way that they are in the reverse order than in the
original. I.e. if the original item has the the codes 1, 2, 3, they are
changed into 3, 2, 1.
}
\usage{
reverse_codes(x)
\S4method{reverse_codes}{item.vector}(x)
}
\arguments{
  \item{x}{An object -- an "item" object or a "data.set" object}
}
\examples{
ds <- data.set(
    x = as.item(sample(1:3,10,replace=TRUE),
                labels=c("One"=1,
                         "Two"=2,
                         "Three"=3)))
ds <- within(ds,{
    xr <- reverse_codes(x)
})
codebook(ds)
}
