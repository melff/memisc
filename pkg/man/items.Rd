\name{items}
\alias{item}
\alias{items}
\alias{item-class}
\alias{item.vector-class}
\alias{integer.item-class}
\alias{double.item-class}
\alias{numeric.item-class}
\alias{character.item-class}
\alias{datetime.item-class}
\alias{Date.item-class}
\alias{as.item}
\alias{as.item,numeric-method}
\alias{as.item,logical-method}
\alias{as.item,character-method}
\alias{as.item,factor-method}
\alias{as.item,ordered-method}
\alias{as.item,POSIXct-method}
\alias{as.item,Date-method}
\alias{as.item,double.item-method}
\alias{as.item,integer.item-method}
\alias{as.item,character.item-method}
\alias{as.item,datetime.item-method}
\alias{as.item,Date.item-method}
\alias{[,item.vector,numeric,missing,missing-method}
\alias{[,item.vector,logical,missing,missing-method}
\alias{[,datetime.item,numeric,missing,missing-method}
\alias{[,datetime.item,logical,missing,missing-method}
\alias{str.double.item}
\alias{str.integer.item}
\alias{str.character.item}
\alias{str.datetime.item}
\alias{unique,item.vector-method}
\alias{as.data.frame.character.item}
\alias{as.data.frame.double.item}
\alias{as.data.frame.integer.item}
\alias{as.vector,item-method}
\alias{as.numeric,item-method}
\alias{as.integer,item-method}
\alias{as.factor,item.vector-method}
\alias{as.ordered,item.vector-method}
\alias{as.character,item.vector-method}
\alias{as.character,datetime.item-method}
\alias{as.character,Date.item-method}
\alias{summary,item.vector-method}
\alias{summary,datetime.item-method}
\alias{summary,Date.item-method}
\alias{format,item.vector-method}
\alias{show,item.vector-method}
\alias{format,datetime.item-method}
\alias{format,Date.item-method}
\alias{show,datetime.item-method}
\alias{show,Date.item-method}
\alias{print,item.vector-method}
\alias{print,datetime.item-method}
\alias{print,Date.item-method}
\alias{Compare,numeric.item,character-method}
\alias{Compare,character,numeric.item-method}
\alias{Arith,numeric.item,numeric.item-method}
\alias{Arith,numeric.item,numeric-method}
\alias{Arith,numeric,numeric.item-method}
\alias{Math,numeric.item-method}
\alias{Math2,numeric.item-method}
\alias{Summary,numeric.item-method}
\alias{\%in\%,numeric.item,character-method}
\alias{rep,item.vector-method}
\alias{weighted.mean,item.vector-method}

\title{Survey Items}
\description{
  Objects of class \code{item} are data vectors with additional information
  attached to them like ``value labels'' and ``user-defined missing values''
  known from software packages like SPSS or Stata.
  }
\usage{
## The constructor for objects of class "item"
## more convenient than new("item",...)
\S4method{as.item}{numeric}(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), \dots
  )
\S4method{as.item}{character}(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), \dots
  )

\S4method{as.item}{logical}(x,\dots)
# x is first coerced to integer,
# arguments in ... are then passed to the "numeric"
# method.

\S4method{as.item}{factor}(x,\dots)
\S4method{as.item}{ordered}(x,\dots)
\S4method{as.item}{POSIXct}(x,\dots)

\S4method{as.item}{double.item}(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), \dots
  )

\S4method{as.item}{integer.item}(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), \dots
  )

\S4method{as.item}{character.item}(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), \dots
  )

\S4method{as.item}{datetime.item}(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), \dots
  )

}

\arguments{
  \item{x}{for \code{as.item} methods,
  any atomic vector; for the \code{as.character}, \code{as.factor},
  \code{as.integer}, \code{as.double}, a vector with class \code{"item"};
  for the \code{unique}, 
  \code{summary}, \code{str}, \code{print}, \code{[}, and \code{<-}
  methods, a vector with class \code{labelled}.
  }
  \item{labels}{a named vector of the same mode as \code{x}.}
  \item{missing.values}{either a vector of the same mode as \code{x},
    or a list with components \code{"values"},
    vector of the same mode as \code{x} (which defines individual missing values)
    and \code{"range"} a matrix with two rows with
    the same mode as \code{x} (which defines a range of missing values),
    or an object of class \code{"missing.values"}.
    }
  \item{valid.values}{either a vector of the same mode as \code{x},
    defining those values of \code{x} that are to be considered as valid,
    or an object of class \code{"valid.values"}.}
  \item{valid.range}{either a vector of the same mode as \code{x} and length 2,
    defining a range of valid values of \code{x},
    or an object of class \code{"valid.range"}.}
  \item{value.filter}{an object of class \code{"value.filter"}, that is, of
     classes \code{"missing.values"}, \code{"valid.values"}, or \code{"valid.range"}.}
  \item{measurement}{level of measurement; one of "nominal", "ordinal", "interval", or "ratio".}
  \item{annotation}{a named character vector,
  or an object of class \code{"annotation"} }
  \item{\dots}{further arguments, ignored.}
}

\seealso{
  \code{\link{annotation}}
  \code{\link{labels}}
  \code{\link{value.filter}}
}
\examples{
  x <- as.item(rep(1:5,4),
      labels=c(
          "First"      = 1,
          "Second"     = 2,
          "Third"      = 3,
          "Fourth"     = 4,
          "Don't know" = 5
        ),
      missing.values=5,
      annotation = c(
        description="test"
      ))
  str(x)
  summary(x)
  as.numeric(x)

  test <- as.item(rep(1:6,2),labels=structure(1:6,
                                      names=letters[1:6]))
  test
  test == 1
  test != 1
  test == "a"
  test != "a"
  test == c("a","z")
  test != c("a","z")
  test %in% 1:3
  test %in% c("a","c")

  codebook(test)

  Test <- as.item(rep(letters[1:6],2),
                    labels=structure(letters[1:6],
                                     names=LETTERS[1:6]))
  Test
  Test == "a"
  Test != "a"
  Test == "A"
  Test != "A"
  Test == c("a","z")
  Test != c("a","z")
  Test %in% c("A","C")
  Test %in% c("a","c")

  as.factor(test)
  as.factor(Test)
  as.numeric(test)
  as.character(test)
  as.character(Test)

  as.data.frame(test)[[1]]
}
\keyword{manip}
