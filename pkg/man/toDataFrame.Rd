\name{to.data.frame}
\alias{to.data.frame}
\title{Convert an Array into a Data Frame}
\description{
  \code{to.data.frame} converts an array into a data frame, in such a way
  that a chosen dimensional extent forms variables in the data frame.
  The elements of the array must be either atomic, data frames
  with matching variables, or coercable into such data frames.
}
\usage{
to.data.frame(X,as.vars=1,name="Freq")
}
\arguments{
  \item{X}{an array.}
  \item{as.vars}{a numeric value or a character string.
    If it is a numeric value then it indicates the dimensional extend
    which defines the variables. If it is a character string then it is
    matched against the names of the dimenstional extents. This is
    applicable e.g. if \code{X} is a contingency table and the
    dimensional extents are named after the cross-classified factors.
    Takes effect only if \code{X} is 
    an atomic array. If \code{as.vars} equals zero, a new variable
    is created that contains the values of the array, that is,
    \code{to.data.frame} acts on the array \code{X}
    like \code{as.data.frame(as.table(X))}
    }
  \item{name}{a character string; the name of the variable
    created if \code{X} is an atomic array and \code{as.vars} equals zero.
    }
}

\value{
  A data frame.
}

\examples{
berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
berktest1 <- By(~Dept+Gender,
                glm(cbind(Admitted,Rejected)~1,family="binomial"),
                data=berkeley)
berktest2 <- By(~Dept,
                glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                data=berkeley)
Stest1 <- Lapply(berktest2,function(x)predict(x,,se.fit=TRUE)[c("fit","se.fit")])
Stest2 <- Sapply(berktest2,function(x)coef(summary(x)))
Stest2.1 <- Lapply(berktest1,function(x)predict(x,,se.fit=TRUE)[c("fit","se.fit")])
to.data.frame(Stest1)
to.data.frame(Stest2,as.vars=2)
to.data.frame(Stest2.1)
# Recasting a contingency table
to.data.frame(UCBAdmissions,as.vars="Admit")
}
\keyword{manip}
