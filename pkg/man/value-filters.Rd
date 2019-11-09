\name{value.filter}
\alias{value.filter}
\alias{value.filter-class}
\alias{value.filter,item-method}
\alias{missing.values-class}
\alias{valid.values-class}
\alias{valid.range-class}
\alias{missing.values}
\alias{missing.values,item.vector-method}
\alias{missing.values<-}
\alias{missing.values<-,ANY,NULL-method}
\alias{missing.values<-,ANY,atomic-method}
\alias{missing.values<-,ANY,list-method}
\alias{missing.values<-,item,NULL-method}
\alias{missing.values<-,item,ANY-method}
\alias{missing.values<-,item,missing.values-method}
\alias{missing.values<-,atomic,missing.values-method}
\alias{valid.values}
\alias{valid.values,item.vector-method}
\alias{valid.values<-}
\alias{valid.values<-,ANY,NULL-method}
\alias{valid.values<-,ANY,atomic-method}
\alias{valid.values<-,item,valid.values-method}
\alias{valid.values<-,atomic,valid.values-method}
\alias{valid.range}
\alias{valid.range,item.vector-method}
\alias{valid.range<-}
\alias{valid.range<-,ANY,NULL-method}
\alias{valid.range<-,ANY,atomic-method}
\alias{valid.range<-,item,valid.range-method}
\alias{valid.range<-,atomic,valid.range-method}
\alias{format,valid.values-method}
\alias{format,valid.range-method}
\alias{format,missing.values-method}
\alias{show,value.filter-method}
\alias{coerce,atomic,valid.values-method}
\alias{coerce,atomic,valid.range-method}
\alias{coerce,atomic,missing.values-method}
\alias{coerce,list,missing.values-method}
\alias{Arith,valid.values,valid.values-method}
\alias{Arith,missing.values,missing.values-method}
\alias{Arith,valid.range,valid.range-method}
\alias{Arith,value.filter,vector-method}
\alias{is.valid}
\alias{is.missing}
\alias{is.missing,item.vector-method}
\alias{is.missing,atomic-method}
\alias{is.missing,factor-method}
\alias{include.missings}
\alias{include.missings,item-method}
\alias{nvalid}
\title{Value Filters}
\description{
  Value filters, that is objects that inherit from class "value.filter",
  are a mechanism to distinguish between valid codes of a survey
  item and codes that are considered to be missing, such as
  the codes for answers like "don't know" or "answer refused".

  Value filters are optional slot values of "item" objects.
  They determine which codes of "item" objects are
  replaced by \code{NA} when they are coerced into
  a vector or a factor.

  There are three (sub)classes of value filters:
  "missing.values", which specify individual
  missing values and/or a range of missing values;
  "valid.values", which specify individual
  valid values (that is, all other values of the
  item are considered as missing);
  "valid.range", which specify a range of
  valid values (that is, all values outside the range
  are considered as missing).
  Value filters of class "missing.values" correspond
  to missing-values declarations in SPSS files,
    imported by \code{\link{spss.fixed.file}},
  \code{\link{spss.portable.file}}, or
  \code{\link{spss.system.file}}.

  Value filters also can be updated using the  \code{+}
  and \code{-} operators.
}
\usage{
value.filter(x)

missing.values(x)
missing.values(x)<-value

valid.values(x)
valid.values(x)<-value

valid.range(x)
valid.range(x)<-value

is.valid(x)
nvalid(x)
is.missing(x)
include.missings(x,mark="*")
}
\arguments{
  \item{x,value}{objects of the appropriate class.}
  \item{mark}{a character string, used to pasted
  to value labels of \code{x} (if present).}
}
\value{
  \code{value.filter(x)}, \code{missing.values(x)}, \code{valid.values(x)}, and \code{valid.range(x)},
  return the value filter associated with \code{x}, an
  object of class "value.filter", that is, of class
  "missing.values", "valid.values", or "valid.range", respectively.

  \code{is.missing(x)} returns a logical vector indicating for
  each element of \code{x} whether it is a missing value or not.
  \code{is.valid(x)} returns a logical vector indicating for
  each element of \code{x} whether it is a valid value or not.
  \code{nvalid(x)} returns the number of elements of \code{x}
  that are valid.
  
  For convenience, \code{is.missing(x)} and \code{is.valid(x)} also work
  for atomic vectors and factors, where they are equivalent to
  \code{is.na(x)} and \code{!is.na(x)}. For atomic vectors and factors,
  \code{nvalid(x)} returns the number of elements of \code{x} for
  which \code{!is.na(x)} is TRUE.

  \code{include.missings(x,...)} returns a copy of \code{x}
  that has all values declared as valid.
}
\examples{
x <- rep(c(1:4,8,9),2,length=60)
labels(x) <- c(
    a=1,
    b=2,
    c=3,
    d=4,
    dk=8,
    refused=9
    )
missing.values(x) <- 9
missing.values(x)
missing.values(x) <- missing.values(x) + 8
missing.values(x)
missing.values(x) <- NULL
missing.values(x)
missing.values(x) <- list(range=c(8,Inf))
missing.values(x)
valid.values(x)
print(x)
is.missing(x)
is.valid(x)
as.factor(x)
as.factor(include.missings(x))
as.integer(x)
as.integer(include.missings(x))
}
