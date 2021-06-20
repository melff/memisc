\name{contract}
\alias{contract}
\alias{contract.data.frame}
\alias{contract.data.set}
\title{Contract data into pattern-frequency format}
\description{
  \code{contract()} contracts data into pattern-frequency format, similar
  to a contatenation of \code{table()} (or \code{xtabs}) and
  \code{as.data.frame()}. Yet it uses much less memory if patterns
  are sparse, because it does not create rows for patterns that do not occur.
}
\usage{
contract(x,\dots)
\method{contract}{data.frame}(x,by=NULL, weights=NULL,name="Freq",
    force.name=FALSE,sort=FALSE,drop.na=TRUE,\dots)
\method{contract}{data.set}(x,by=NULL, weights=NULL,name="Freq",
    force.name=FALSE,sort=FALSE,drop.na=TRUE,\dots)
}
\arguments{
  \item{x}{
    an object of class \code{"data.frame"} or \code{"data.set"}.
  }
  \item{by}{the formula or a vector of variable names (quoted or not quoted).
    Specifies the patterns (and optionally weights).
    If \code{by} is a formula, then the right-hand side specifies the
    variables the value patterns of which are counted.
    If the left-hand side of the formula is (the name of) a numeric
    vector, its values are used as weights (in which case the
    \code{weights} argument will be ignored.) If the left-hand side of
    the formula is (the name of) a factor, counts are computed  in
    separate columns for each of its levels.
  }
  \item{weights}{a numeric vector of weights or \code{NULL}.}
  \item{name}{a character string, the name of the variable that
    containts the frequency counts of the value patterns.}
  \item{force.name}{a logical value, defaults to \code{FALSE}. If
    \code{TRUE} and the left-hand side of \code{by} formula is a factor,
    the names of the columns with the counts are combinations of the
    labels of the factor levels and the argument of \code{name}; if
    \code{FALSE}, the column names are created from the labels of the
    factor levels only.
  }
  \item{sort}{a logical value, defaults to \code{FALSE}. If \code{TRUE},
    the resulting data set is sorted by the variables that define the
    patterns. If \code{FALSE}, the row of the resulting data frame or
    data set are ordered according to the occurrence of the patterns. 
  }
  \item{drop.na}{a logical value, defaults to \code{TRUE}. If
    \code{FALSE}, patterns that involve \code{NA} are included in the
    resulting data frame or data set.}
  \item{\dots}{further arguments, passed to methods or ignored.}
}
\value{
  If \code{x} is a data fame, the value of \code{contract()} is also a
  data frame. If it is a \code{"data.set"} object, the result is also a
  \code{"data.set"} object. 
}
\examples{
iris_ <- sample(iris,size=nrow(iris),replace=TRUE)
w <- rep(1,nrow(iris_))
contract(iris[4:5])
contract(iris[4:5],sort=TRUE)
contract(iris[4:5],weights=w,sort=TRUE)
contract(iris,by=c(Petal.Width,Species),sort=TRUE)
contract(iris,by=~Petal.Width+Species)
contract(iris,by=w~Species)

library(MASS)
contract(housing,
         by=Sat~Infl+Type+Cont,
         weights=Freq)

contract(housing,
         by=Sat~Infl+Type+Cont,
         weights=Freq,
         name="housing",force.name=TRUE
         )
}
