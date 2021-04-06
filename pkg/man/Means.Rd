\name{Means}
\alias{Means}
\alias{Means.data.frame}
\alias{Means.numeric}
\alias{Means.formula}
\alias{as.data.frame.means.table}
\alias{as.data.frame.xmeans.table}
\title{Means for groups of observations}
\description{The function \code{Means()} creates a table of group
  means, optionally with standard errors, confidence intervals, and
  numbers of valid observations.}
\usage{
Means(data, \dots)
\method{Means}{data.frame}(data,
    by, weights=NULL, subset=NULL,
    default=NA,
    se=FALSE, ci=FALSE, ci.level=.95,
    counts=FALSE, \dots)
\method{Means}{formula}(data, subset, weights, \dots)
\method{Means}{numeric}(data, \dots)
\method{as.data.frame}{means.table}(x, row.names=NULL, optional=TRUE, drop=TRUE, \dots)
\method{as.data.frame}{xmeans.table}(x, row.names=NULL, optional=TRUE, drop=TRUE, \dots)
}
\arguments{
  \item{data}{an object usually containing data, or a formula.

    If \code{data} is a numeric vector or an object that can be coerced
    into a data frame, it is changed into a data frame and the data
    frame method of \code{Means()} is applied to it.

    If \code{data} is a formula, then a data frame is constructed from
    the variables in the formula and \code{Means} is applied to this
    data frame, while the formula is passed on as a \code{by=} argument.
  }
  \item{by}{a formula, a vector of variable names or a data frame or
    list of factors.

    If \code{by} is a vector of variable names,
    they are extracted from \code{data} to define the groups for which
    means are computed, while the variables for which the means are
    computed are those not named in \code{by}.

    If \code{by} is a data frame or a list of factors,
    these are used to defined the groups for which means are computed,
    while the variables for which the means are
    computed are those not in \code{by}.

    If \code{by} is a formula, its left-hand side determines the
    variables of which means are computed, while its right-hand side
    determines the factors that define the groups.
  }
  \item{weights}{an optional vector of weights, usually a variable in \code{data}.}
  \item{subset}{an optional logical vector to select observations,
    usually the result of an expression in variables from \code{data}.}
  \item{default}{a default value used for empty cells without
    observations.}
  \item{se}{a logical value, indicates whether standard errors should be
    computed.}
  \item{ci}{a logical value, indicates whether limits of confidence
    intervals should be computed.}
  \item{ci.level}{a number, the confidence level of the confidence interval}
  \item{counts}{a logical value, indicates whether numbers of valid
    observations should be reported.}
  \item{x}{for \code{as.data.frame()}, a result of \code{Means()}.}
  \item{row.names}{an optional character vector. This argmument presently is
    inconsequential and only included for reasons of compatiblity
    with the standard methods of \code{\link[base]{as.data.frame}}.}
  \item{optional}{an optional logical value. This argmument presently is
    inconsequential and only included for reasons of compatiblity
    with the standard methods of \code{\link[base]{as.data.frame}}.}
  \item{drop}{a logical value, determines whether "empty cells" should
    be dropped from the resulting data frame.}
  \item{\dots}{other arguments, either ignored or passed on to other
    methods where applicable.}
}
\value{
  An array that inherits classes "means.table" and "table". If
  \code{Means} was called with \code{se=TRUE} or \code{ci=TRUE}
  then the result additionally inherits class "xmeans.table".
}
\examples{
# Preparing example data
USstates <- as.data.frame(state.x77)
USstates <- within(USstates,{
    region <- state.region
    name <- state.name
    abb <- state.abb
    division <- state.division
})
USstates$w <- sample(runif(n=6),size=nrow(USstates),replace=TRUE)

# Using the data frame method
Means(USstates[c("Murder","division","region")],by=c("division","region"))
Means(USstates[c("Murder","division","region")],by=USstates[c("division","region")])
Means(USstates[c("Murder")],1)
Means(USstates[c("Murder","region")],by=c("region"))

# Using the formula method
# One 'dependent' variable
Means(Murder~1, data=USstates)
Means(Murder~division, data=USstates)
Means(Murder~division, data=USstates,weights=w)
Means(Murder~division+region, data=USstates)
as.data.frame(Means(Murder~division+region, data=USstates))

# Standard errors and counts
Means(Murder~division, data=USstates, se=TRUE, counts=TRUE)
drop(Means(Murder~division, data=USstates, se=TRUE, counts=TRUE))
as.data.frame(Means(Murder~division, data=USstates, se=TRUE, counts=TRUE))

# Confidence intervals
Means(Murder~division, data=USstates, ci=TRUE)
drop(Means(Murder~division, data=USstates, ci=TRUE))
as.data.frame(Means(Murder~division, data=USstates, ci=TRUE))

# More than one dependent variable
Means(Murder+Illiteracy~division, data=USstates)
as.data.frame(Means(Murder+Illiteracy~division, data=USstates))

# Confidence intervals
Means(Murder+Illiteracy~division, data=USstates, ci=TRUE)
as.data.frame(Means(Murder+Illiteracy~division, data=USstates, ci=TRUE))

# Some 'non-standard' but still valid usages:
with(USstates,
     Means(Murder~division+region,subset=region!="Northeast"))

with(USstates,
     Means(Murder,by=list(division,region)))
}
