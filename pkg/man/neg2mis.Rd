\name{neg2mis}
\alias{neg2mis}
\title{Mark Negative Values as Missing}
\description{In many newer survey data sets available from social
  science data archives non-valid responses (such as "don't know" or
  "answer refused") are given negative codes. The function
  \code{neg2miss} allows to mark them as missing values.)
}
\usage{
neg2mis(x,all=FALSE,exclude=NULL,select=NULL,zero=FALSE)
}
\arguments{
  \item{x}{an object that inherits from class "item.list", e.g. a
    "data.set" or an "importer" object.}
  \item{all}{logical; should the marking of negative values as missing
    applied to all variables?}
  \item{exclude}{an optional vector of variable naems to which
    the marking of negative values as missing should \emph{not} be
    applied.}
  \item{select}{an optional vector of variable names to which
    the marking of negative values as missing should be applied.}
  \item{zero}{logical; should zeroes also be marked as missing?}
}
\examples{
ds <- data.set(
          var1 = c(0,1,-1,2,3),
          var2 = c(-1,-1,1,1,1),
          var3 = c(1,2,3,4,5)
          )
neg2mis(ds,all=TRUE)
neg2mis(ds,all=TRUE,zero=TRUE)
neg2mis(ds,exclude=var1)
neg2mis(ds,select=var1)
}
