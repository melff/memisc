\name{duplicated_labels}
\alias{duplicated_labels}
\alias{duplicated_labels.item}
\alias{duplicated_labels.item.list}
\title{Check for and report duplicated labels}
\description{
  The function \code{duplicated_labels} can be used with "item" objects,
  "importer" objects or "data.set" objects to check whether items
  contain duplicate labels, i.e. labels that are attached to more than
  one code.
}
\usage{
duplicated_labels(x)
\S3method{duplicated_labels}{item}(x)
# Applicable to 'importer' objects and 'data.set' objects
\S3method{duplicated_labels}{item.list}(x)
}
\arguments{
  \item{x}{an item with value labels or that contains items with
    value labels}
}
\value{The function \code{duplicate.labels} returns a list with a class
  attribute, which allows pretty printing of duplicated value labels}
\examples{
x1 <- as.item(rep(1:5,4),
              labels=c(
                  A = 1,
                  A = 2,
                  B = 3,
                  B = 4,
                  C = 5
              ),
              annotation = c(
                  description="Yet another test"
))
              
x2 <- as.item(rep(1:4,5),
              labels=c(
                  i   = 1,
                  ii  = 2,
                  iii = 3,
                  iii = 4
                  ),
              annotation = c(
                  description="Still another test"
))

x3 <- as.item(rep(1:2,10),
              labels=c(
                  a = 1,
                  b = 2
                  ),
              annotation = c(
                  description="Still another test"
))
                            
duplicated_labels(x1)
ds <- data.set(x1,x2,x3)
duplicated_labels(ds)
codebook(ds)

nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())
nes1948 <- spss.portable.file(nes1948.por)
duplicated_labels(nes1948)             
}