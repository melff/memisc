\name{By}
\alias{By}
\title{Conditional Evaluation of an Expression}
\description{
  The function \code{By} evaluates an expression within subsets of
  a data frame, where the subsets are defined by a formula.
  }
\usage{
By(formula,expr,data=parent.frame())
}
\arguments{
  \item{formula}{an expression or (preferably) a formula
      containing the names of conditioning variables or factors.}
  \item{expr}{an expression that is evaluated for any unique combination
    of values of the variables contained in \code{formula}.}
  \item{data}{a data frame, an object that can be coerced into
    a data frame (for example, a \link{table}), or an environment,
    from which values for the variables in \code{formula}
    or \code{expr} are taken.
    }
}
\value{
  A list of class "by", giving the results for each combination of values
  of variables in \code{formula}.
}
\examples{

berkeley <- aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
(Bres <- By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial"),data=berkeley))
# The results all have 'data' components
str(Bres[[1]]$data)

attach(berkeley)
(Bres <- By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial")))
detach(berkeley)

}
\keyword{manip}
\keyword{univar}
