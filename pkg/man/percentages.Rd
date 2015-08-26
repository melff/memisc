\name{percentages}
\alias{percentages}
\alias{percentages.formula}
\alias{percentages.table}
\alias{as.data.frame.percentage.table}
\alias{as.data.frame.xpercentage.table}
\title{
  Easy Creation of Tables of Percentages
}
\description{
  The generic function \code{percentages} and its methods
  create one- or multidimensional tables of percentages. As such,
  the function \code{percentages} can be viewed as a convenience
  interface to \code{\link{prop.table}}. However, it also
  allows to obtain standard errors and confidence intervals.
}
\usage{
percentages(obj, \dots)
\method{percentages}{table}(obj,
      by=NULL, which=NULL, se=FALSE, ci=FALSE, ci.level=.95, \dots)
\method{percentages}{formula}(obj,
      data=parent.frame(), weights=NULL, \dots)
\method{as.data.frame}{percentage.table}(x, \dots)
\method{as.data.frame}{xpercentage.table}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object; a contingency table or a formula. If
  it is a formula, its left-hand side determines the factor 
  or combination of factors for which percentages are 
  computed while its right-hand side determines the 
  factor or combination of factors that define the 
  groups within which percentages are computed.}
  \item{by}{a character vector with the names of the
  factor variables that define the groups within which percentages
  are computed. Percentages sum to 100 within combination
  of levels of these factors.}
  \item{which}{a character vector with the names of the
  factor variables for which percentages 
  are computed.}
  \item{se}{a logical value; determines whether standard
  errors are computed.}
  \item{ci}{a logicla value; determines whether confidence
  intervals are computed. Note that the confidence intervals
  are for infinite (or very large) populations.}
  \item{ci.level}{a numerical value, the required confidence level of
  the confidence intervals.}
  \item{data}{a contingency table (an object that inherits from "table")
   or a data frame or an object coercable into a data frame.}
  \item{weights}{an optional character string with the name 
  of a variable containing weights.}
  \item{x}{an object coerced into a data frame.}
  \item{\dots}{Further arguments, ignored.}
}
\value{
  An array that inherits classes "percentage.table" and "table". If
  \code{percentages} was called with \code{se=TRUE} or \code{ci=TRUE}
  then the result additionally inherits class "xpercentage.table".
}
\examples{

# Two equivalent ways to create the same table
100*prop.table(UCBAdmissions)
percentages(UCBAdmissions)

# Three equivalent ways to create the same table
100*prop.table(UCBAdmissions,c(2,3))
(p0 <- percentages(UCBAdmissions,by=c("Gender","Dept")))
percentages(UCBAdmissions,which="Admit")
# Percentage table as data frame
as.data.frame(p0)

# Three equivalent ways to create the same table
100*prop.table(margin.table(UCBAdmissions,c(1,2)),2)
percentages(UCBAdmissions,which="Admit",by="Gender")
percentages(Admit~Gender,data=UCBAdmissions)

# Three equivalent ways to create the same table
100*prop.table(margin.table(UCBAdmissions,c(1,3)),2)
percentages(Admit~Dept,data=UCBAdmissions)
percentages(Admit~Dept,data=as.data.frame(UCBAdmissions),
            weights="Freq")

# Standard errors and confidence intervals
percentages(Admit~Dept,data=UCBAdmissions,se=TRUE)
percentages(Admit~Dept,data=UCBAdmissions,ci=TRUE)
(p<- percentages(Admit~Dept,data=UCBAdmissions,ci=TRUE,se=TRUE))

# An extended table of percentages as data frame
as.data.frame(p)
}
