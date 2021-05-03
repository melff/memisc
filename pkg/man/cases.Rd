\name{cases}
\alias{cases}
\title{Distinguish between Cases Specified by Logical Conditions}
\description{
  \code{cases} allows to distinguish several cases defined logical
  conditions. It can be used to code these cases into a vector. The
  function can be considered as a multi-condition generalization of
  \code{\link{ifelse}}.
  }
\usage{
cases(\dots,check.xor=c("warn","stop","ignore"),
      .default=NA,.complete=FALSE,
      check.na=c("warn","stop","ignore"),
      na.rm=TRUE)
}
\arguments{
  \item{\dots}{A sequence of logical expressions or assignment expressions containing
    logical expressions as "right hand side".}
  \item{check.xor}{character (either \code{"warn"}, \code{"stop"}, or  \code{"ignore"})
    or logical; if \code{TRUE} or equal to \code{"stop"} or 
    \code{"warn"}, 
    \code{cases} checks whether the case
    conditions are mutually exclusive. If this is not
    satisfied and \code{check.xor} equals \code{"warn"} (the default), a warning is shown,
    otherwise an error exception is raised.}
  \item{.default}{a value to be used for unsatisfied conditions.}
  \item{.complete}{logical, if \code{TRUE} an additional factor level is
    created for the unsatisfied conditions.}
  \item{check.na}{character (either \code{"warn"}, \code{"stop"}, or  \code{"ignore"})
    or logical; if \code{TRUE} or equal to \code{"stop"} or 
    \code{"warn"}, \code{cases} checks, whether any of the case
    conditions evaluates to \code{NA}.
    If that case, if \code{check.na} is \code{TRUE} or equals
    \code{"stop"} an error exception is raised, while if \code{check.na}
    equals \code{"warn"} (the default) a warning is shown.}
  \item{na.rm}{a logical value; how to handle \code{NA}s (if they do not
    already lead to an error exception). If \code{FALSE} if \emph{any} of the
    conditions evaluates to \code{NA}, the corresponding value of the
    result vector is \code{NA}. If \code{TRUE} (the default), the
    resulting vector or factor is \code{NA} only for instances where all
    conditions result in \code{NA}. 
  }
}
\details{
  There are two distinct ways to use this function. Either the
  function can be used to construct a factor that represents
  several logical cases or it can be used to conditionally
  evaluate an expression in a manner similar to \code{\link{ifelse}}.

  For the first use, the \code{\dots} arguments have to be a series of
  logical expressions. \code{cases} then returns a factor
  with as many levels as logical expressions given as
  \code{\dots} arguments. The resulting factor will attain its
  first level if the first condition is TRUE, otherwise it will attain its
  second level if the second condition is TRUE, etc.
  The levels will be named after the conditions or, if name tags are
  attached to the logical expressions, after the tags of the expressions.
  Not that the logical expressions all need to evaluate to logical vectors
  of the same length, otherwise an error condition is raised.
  If \code{.complete} is \code{TRUE} then an additional factor level is
  created for the conditions not satisfied for any of the cases.
  
  For the second use, the \code{\dots} arguments have to be a series
  of assignment expression of the type \code{<expression> <- <logical expression>}
  or \code{<logical expression> -> <expression>}. For cases
  in which the first logical expression is TRUE, the result of first expression that
  appears on the other side of the assignment operator become elements of the
  vector returned by \code{cases}, for cases in which the second logical expression is TRUE,
  the result of the second expression that appears on the other side
  of the assignment operator become elements of the
  vector returned by \code{cases}, etc.
  For cases that do not satisfy any of the given conditions the value of
  the \code{.default} argument is used. 
  Note that the logical expressions also here all need to evaluate to logical
  vectors of the same length. The expressions on the other side of the
  assignment operator should also be either vectors of the same length
  and mode or should scalars of the same mode, otherwise unpredictable
  results may occur.

}
\value{
  If it is called with logical expressions as \dots arguments,
  \code{cases} returns a factor, if it is called with
  assignment expressions the function returns a vector with the
  same mode as the results of the "assigned" expressions
  and with the same length as the logical conditions.
}

\examples{
# Examples of the first kind of usage of the function
#
df <- data.frame(x = rnorm(n=20), y = rnorm(n=20))
df <- df[do.call(order,df),]
(df <- within(df,{
  x1=cases(x>0,x<=0)
  y1=cases(y>0,y<=0)
  z1=cases(
    "Condition 1"=x<0,
    "Condition 2"=y<0,# only applies if x >= 0
    "Condition 3"=TRUE
    )
  z2=cases(x<0,(x>=0 & y <0), (x>=0 & y >=0))
  }))
xtabs(~x1+y1,data=df)
dd <- with(df,
  try(cases(x<0,
            x>=0,
            x>1,
            check.xor=TRUE)# let's be fussy
            )
  )
dd <- with(df,
  try(cases(x<0,x>=0,x>1))
  )
genTable(range(x)~dd,data=df)

# An example of the second kind of usage of the function:
# A construction of a non-smooth function
#
fun <- function(x)
  cases(
    x==0      -> 1,
    abs(x)> 1 -> abs(x),
    abs(x)<=1 -> x^2
  )
x <- seq(from=-2,to=2,length=101)
plot(fun(x)~x)

# Demo of the new .default and .complete arguments
x <- seq(from=-2,to=2)
cases(a = x < -1,
      b = x > 1,
      .complete = TRUE)
cases(x < -1,
      x > 1,
      .complete = TRUE)
cases(1 <- x < -1,
      3 <- x > 1,
      .default = 2)

threshhold <- 5
d <- c(1:10, NaN)

d1 <- cases(
  d > threshhold -> 1,
  d <= threshhold -> 2
)

d2 <- cases(
  is.na(d) -> 0,
  d > threshhold -> 1,
  d <= threshhold -> 2
)

# Leads to missing values because some of the conditions result in missing
# even though they could be 'captured'
d3 <- cases(
  is.na(d) -> 0,
  d > threshhold -> 1,
  d <= threshhold -> 2,
  na.rm=FALSE
)

d4 <- cases(
  is.na(d) -> 0,
  d > threshhold +2 -> 1,
  d <= threshhold -> 2,
  na.rm=FALSE
)

cbind(d,d1,d2,d3,d4)

cases(
  d > threshhold,
  d <= threshhold
)

cases(
  is.na(d),
  d > threshhold,
  d <= threshhold
)

cases(
  d > threshhold,
  d <= threshhold,
  .complete=TRUE
)

cases(
  d > threshhold + 2,
  d <= threshhold,
  .complete=TRUE
)
}

\keyword{manip}
