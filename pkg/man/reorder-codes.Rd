\name{reorder_codes}
\alias{reorder_codes}
\alias{reorder_codes,item.vector-method}
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
reorder_codes(x,from,to)
\S4method{reorder_codes}{item.vector}(x,from,to)

reverse_codes(x,from,to)
\S4method{reverse_codes}{item.vector}(x,from,to)
}
\arguments{
  \item{x}{An object -- an "item" object or a "data.set" object}
  \item{from}{original codes, a numeric or character vector.}
  \item{to}{new codes, of the same length and type as \code{from}.}
}
\examples{
ds <- data.set(
    x = as.item(sample(1:3,10,replace=TRUE),
                labels=c("One"=1,
                         "Two"=2,
                         "Three"=3)))
ds <- within(ds,{
    x1 <- reorder_codes(x,from=1:3,to=3:1)
    x3 <- reorder_codes(x,from=1:3,to=c(2,3,1))
    x2 <- reorder_codes(x,from=1:2,to=2:1)
    xr <- reverse_codes(x)
})
codebook(ds)
}
