\name{deduplicate_labels}
\alias{deduplicate_labels}
\alias{deduplicate_labels.item}
\alias{deduplicate_labels.item.list}
\title{Handle duplicated labels}
\description{
  The function \code{deduplicate_labels} can be used with "item" objects,
  "importer" objects or "data.set" objects to deal with 
  duplicate labels,
  i.e. labels that are attached to more than
  one code. There are several ways to de-duplicate labels: by combining
  values that share their label or by making labels duplicate labels distinct.
}
\usage{
deduplicate_labels(x,\dots)
\S3method{deduplicate_labels}{item}(x,
    method=c("combine codes",
             "prefix values",
             "postfix values"),\dots)
# Applicable to 'importer' objects and 'data.set' objects
\S3method{deduplicate_labels}{item.list}(x,\dots)
}
\arguments{
  \item{x}{an item with value labels or that contains items with
    value labels}
  \item{method}{a character string that determines the method to
    make value labels unique.}
  \item{\dots}{other arguments, passed to specific methods of the
  generic function.}
}
\value{The function \code{deduplicate_labels} a copy of \code{x}
  that has unqiue value labels.
}
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
                            
codebook(deduplicate_labels(x1))
codebook(deduplicate_labels(x1,method="prefix"))
codebook(deduplicate_labels(x1,method="postfix"))

ds <- data.set(x1,x2,x3)
codebook(deduplicate_labels(ds))
codebook(deduplicate_labels(ds,method="prefix"))
codebook(deduplicate_labels(ds,method="postfix"))

}
