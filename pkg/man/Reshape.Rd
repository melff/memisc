\name{Reshape}
\alias{Reshape}
\title{Reshape data frames or data sets}
\description{\code{Reshape} is a convenience
  wrapper around \code{\link[stats]{reshape}} with a somewhat simpler
  syntax.}
\usage{
Reshape(data,\dots,id,within_id,drop,keep,direction)
}
\arguments{
  \item{data}{a data frame or data set to be reshaped.}
  \item{\dots}{
    Further arguments that specify the variables in
    long and in wide format as well as the time variable.
    The name tags of the arguments given here specify 
    variable names in long format,
    the arguments themselves specify the variables in wide format
    (or observations in long vormat)
    and the variable of the "time" variable.
    The time variable is usually the last of these arguments.
    An "automatic" time variable can be specified if only
    a single argument in \code{\dots} is given.
  }
  \item{id}{a variable name or a concatenation of variable names
      (either as character strings or as unquoted symbols), that identify
      individual units. Defaults to \code{"id"} or the id variable
      specified in the \code{"reshapeLong"} attribute of the \code{data}
      argument. Needed only if the data are reshaped from long to wide
      format.
  }
  \item{within_id}{an optional variable name 
      (either as character string or as unquoted symbol), that identifies
      individual observations on units.
      Relevant only if the data are reshaped from long to wide
      format.}
  \item{drop}{a variable name or a concatenation of variable names
      (either as character strings or as unquoted symbols), thast specifies
      the variables to be dropped before reshaping.
  }
  \item{keep}{a variable name or a concatenation of variable names
      (either as character strings or as unquoted symbols), thast specifies
      the variables to be kept after reshaping (including the ones
      used to define the reshaping).
  }
  \item{direction}{a character string, should be either equal "long"
    or "wide".}
}
\examples{
example.data.wide <- data.frame(
    v  = c(35,42),
    x1 = c(1.1,2.1),
    x2 = c(1.2,2.2),
    x3 = c(1.3,2.3),
    x4 = c(1.4,2.4),
    y1 = c(2.5,3.5),
    y2 = c(2.7,3.7),
    y3 = c(2.9,3.9))
example.data.wide

# The following two calls are equivalent:
example.data.long <- Reshape(data=example.data.wide,
                             x=c(x1,x2,x3,x4),
                             # N.B. it is possible to
                             # specify 'empty' i.e. missing
                             # measurements
                             y=c(y1,y2,y3,),
                             t=1:4,
                             direction="long")

example.data.long <- Reshape(data=example.data.wide,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 # N.B. it is possible to
                                 # specify 'empty' i.e. missing
                                 # measurements
                                 y=c(y1,y2,y3,)
                             ),
                             t=1:4,
                             direction="long")

example.data.long

# Since the data frame contains an "reshapeLong" attribute
# an id variable is already specified and part of the data
# frame.
example.data.wide <- Reshape(data=example.data.long,
                             x=c(x1,x2,x3,x4),
                             y=c(y1,y2,y3,),
                             t=1:4,
                             direction="wide")
example.data.wide

# Here we examine the case where no "reshapeLong" attribute
# is present:
example.data.wide <- Reshape(data=example.data.long,
                             x=c(x1,x2,x3,x4),
                             y=c(y1,y2,y3,),
                             t=1:4,
                             id=v,
                             direction="wide")
example.data.wide


# Here, an "automatic" time variable is created. This works
# only if there is a single argument other than the data=
# and direction= arguments
example.data.long <- Reshape(data=example.data.wide,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,)
                             ),
                             direction="long")

example.data.long

example.data.wide <- Reshape(data=example.data.long,
                             list(
                                 x=c(x1,x2,x3,x4),
                                 y=c(y1,y2,y3,)
                             ),
                             direction="wide")
example.data.wide
}
