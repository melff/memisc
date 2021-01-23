\name{data.set manipulation}
\alias{subset.data.set}
\alias{unique,data.set-method}
\alias{merge,data.set,data.set-method}
\alias{merge,data.set,data.frame-method}
\alias{merge,data.frame,data.set-method}
\alias{cbind.data.set}
\alias{rbind.data.set}
\title{Manipulation of Data Sets}
\description{
  Like data frames, \code{data.set} objects have
  \code{\link[base]{subset}}, \code{\link[base]{unique}},
  \code{\link[base]{cbind}}, \code{\link[base:cbind]{rbind}},
  \code{\link[base]{merge}} methods defined for them.

  The semantics are basically the same as the methods defined
  for data frames in the \code{base} package, with the only difference
  that the return values are \code{data.set} objects.
  In fact, the methods described here are front-ends to the
  corresponding methods for data frames, which are constructed
  such that the "extra" information attached to variables within
  \code{data.set} objects, that is, to \code{item} objects.
}
\usage{
\method{subset}{data.set}(x, subset, select, drop = FALSE, \dots)

\S4method{unique}{data.set}(x, incomparables = FALSE, \dots)

\S3method{cbind}{data.set}(\dots, deparse.level = 1)

\S3method{rbind}{data.set}(\dots, deparse.level = 1)

\S4method{merge}{data.set,data.set}(x,y, \dots)

\S4method{merge}{data.set,data.frame}(x,y, \dots)

\S4method{merge}{data.frame,data.set}(x,y, \dots)
}
\arguments{
  \item{x,y}{\code{data.set} objects. On of the arguments to
    \code{merge} may also be an object coercable into a data frame
    and the result still is a \code{data.set} object.
  }
  \item{subset}{a logical expression, used to select observations from
    the data set.}
  \item{select}{a vector with variablen names, which are retained in the
    data subset.}
  \item{drop}{logical; if \code{TRUE} and the result has only one
    column, the result is an item and not a data set.}
  \item{\dots}{for \code{subset}: a logical vector
    of the same length as the number of rows of the \code{data.set}
    and, optionally, a vector of variable names (tagged as \code{select});
    for \code{unique}: further arguments, ignored;
    for \code{cbind}, \code{rbind}: objects coercable
    into data frames, with at least one being a \code{data.set}
    object;
    for \code{merge}: further arguments
    such as arguments tagged with \code{by}, \code{by.x}, \code{by.y},
    etc. that specify the variables by which to merge
    the data sets of data frames \code{x} and \code{y}.
  }
  \item{incomparables}{a vector of values that cannot be compared. See
    \code{\link[base]{unique}}.
  }
  \item{deparse.level}{an argument retained for
    reasons of compatibility of the default methods
    of \code{\link[base]{cbind}} and \code{\link[base:cbind]{rbind}}.
  }
}
\examples{
ds1 <- data.set(
      a = rep(1:3,5),
      b = rep(1:5,each=3)
  )
ds2 <- data.set(
      a = c(3:1,3,3),
      b = 1:5
  )

ds1 <- within(ds1,{
      description(a) <- "Example variable 'a'"
      description(b) <- "Example variable 'b'"
  })

ds2 <- within(ds2,{
      description(a) <- "Example variable 'a'"
      description(b) <- "Example variable 'b'"
  })

str(ds3 <- rbind(ds1,ds2))
description(ds3)

ds3 <- within(ds1,{
        c <- a
        d <- b
        description(c) <- "Copy of variable 'a'"
        description(d) <- "Copy of variable 'b'"
        rm(a,b)
    })
str(ds4 <- cbind(ds1,ds3))
description(ds4)

ds5 <- data.set(
        c = 1:3,
        d = c(1,1,2)
        )
ds5 <- within(ds5,{
      description(c) <- "Example variable 'c'"
      description(d) <- "Example variable 'd'"
  })
str(ds6 <- merge(ds1,ds5,by.x="a",by.y="c"))

# Note that the attributes of the left-hand variables
# have priority.
description(ds6)
}
