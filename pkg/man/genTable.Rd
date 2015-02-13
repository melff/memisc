\name{genTable}
\alias{Aggregate}
\alias{genTable}
\alias{fapply}
\alias{fapply.default}
\title{Generic Tables and Data Frames of Descriptive Statistics}
\description{
  \code{genTable} creates a table of arbitrary summaries conditional on
  given values of independent variables given by a formula.

  \code{Aggregate} does the same, but returns a \code{data.frame} instead.

  \code{fapply} is a generic function that dispatches on its \code{data}
  argument. It is called internally by \code{Aggregate} and \code{genTable}.
  Methods for this function can be used to adapt \code{Aggregate} and
  \code{genTable} to data sources other than data frames.
  }
\usage{
Aggregate(formula, data=parent.frame(), subset=NULL,
      sort = TRUE, names=NULL, addFreq=TRUE, as.vars=1,
      drop.constants=TRUE,\dots)

genTable(formula, data=parent.frame(), subset=NULL,
      names=NULL, addFreq=TRUE,\dots)

fapply(formula,data,\dots) # calls UseMethod("fapply",data)
\method{fapply}{default}(formula, data, subset=NULL,
      names=NULL, addFreq=TRUE,\dots)
}
\arguments{
  \item{formula}{a formula. The right hand side includes one or more
  grouping variables separated by '+'. These may be factors, numeric,
  or character vectors. The left hand side may be empty,
  a numerical variable, a factor, or an expression.
  See details below.}
  \item{data}{an environment or data frame or an object coercable into a data frame.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{sort}{a logical value; determines the order in which the aggregated
    data appear in the data frame returned by \code{Aggregate}.
    If \code{sort} is TRUE, then the returned data frame is sorted by
    the values of the grouping variables, if \code{sort} is FALSE,
    the order of resulting data frame corresponds to the order in which
    the values of the grouping variables appear in the original data
    frame. }
  \item{names}{an optional character vector giving names to the
    result(s) yielded by the expression on the left hand side of \code{formula}.
    This argument may be redundant if the left hand side results in is a named vector.
    (See the example below.)}
  \item{addFreq}{a logical value. If TRUE and
      \code{data} is a table or a data frame with a variable
      named "Freq", a call to
      \code{table}, \code{\link{Table}}, \code{\link{percent}}, or \code{\link{nvalid}}
      is supplied by an additional argument \code{Freq}
      and a call to \code{table} is translated into
      a call to \code{Table}.
    }
  \item{as.vars}{an integer; relevant only if the left hand side of the formula returns
    an array or a matrix - which dimension (rows, columns, or layers etc.) will transformed to
    variables? Defaults to columns in case of matrices and to the highest dimensional extend
    in case of arrays.}
  \item{drop.constants}{logical; variables that are constant across levels dropped from the
    result?}
  \item{\dots}{further arguments, passed to methods or ignored.}
}
\details{
    If an expression is given as left hand side of the formula, its
    value is computed for any combination of values of the values on the
    right hand side. If the right hand side is a dot, then all
    variables in \code{data} are added to the right hand side of the
    formula.

    If no expression is given as left hand side,
    then the frequency counts for the respective
    value combinations of the right hand variables are computed.

    If a single factor is on the left hand side, then the left hand side is
    translated into an appropriate
    call to \code{table()}. Note that also in this case \code{addFreq} takes effect.

    If a single numeric variable is on the left hand side, frequency
    counts weighted by this variable are computed. In these cases,
    \code{genTable} is equivalent to \code{\link[stats]{xtabs}} and
    \code{Aggregate} is equivalent to \code{as.data.frame(xtabs(\dots))}.
}
\value{
    \code{Aggregate}
    results in a data frame with conditional summaries and unique value combinations
    of conditioning variables.

    \code{genTable} returns a \link{table}, that is, an array with class \code{"table"}.
}
\seealso{\link{aggregate.data.frame}, \link{xtabs}}
\examples{
ex.data <- expand.grid(mu=c(0,100),sigma=c(1,10))[rep(1:4,rep(100,4)),]
ex.data <- within(ex.data,
                  x<-rnorm(
                    n=nrow(ex.data),
                    mean=mu,
                    sd=sigma
                    )
                  )

Aggregate(~mu+sigma,data=ex.data)
Aggregate(mean(x)~mu+sigma,data=ex.data)
Aggregate(mean(x)~mu+sigma,data=ex.data,name="Average")
Aggregate(c(mean(x),sd(x))~mu+sigma,data=ex.data)
Aggregate(c(Mean=mean(x),StDev=sd(x),N=length(x))~mu+sigma,data=ex.data)
genTable(c(Mean=mean(x),StDev=sd(x),N=length(x))~mu+sigma,data=ex.data)

Aggregate(table(Admit)~.,data=UCBAdmissions)
Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
Aggregate(Admit~.,data=UCBAdmissions)
Aggregate(percent(Admit)~.,data=UCBAdmissions)
Aggregate(percent(Admit)~Gender,data=UCBAdmissions)
Aggregate(percent(Admit)~Dept,data=UCBAdmissions)
Aggregate(percent(Gender)~Dept,data=UCBAdmissions)
Aggregate(percent(Admit)~Dept,data=UCBAdmissions,Gender=="Female")
genTable(percent(Admit)~Dept,data=UCBAdmissions,Gender=="Female")
}
\keyword{misc}
