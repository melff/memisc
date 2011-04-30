\name{percent}
\alias{percent}
\alias{percent.default}
\alias{percent.logical}
\title{Table of Percentages with Percentage Base}
\description{
  \code{percent} returns a table of percentages along with
  the percentage base. It will be useful
  in conjunction with \code{\link{aggregate.formula}}.
}
\usage{
  percent(x,\dots)
  \method{percent}{default}(x,weights=NULL,total=!(se || ci),
      se=FALSE,ci=FALSE,ci.level=.95,
      total.name="N",perc.label="Percentage",\dots)
  \method{percent}{logical}(x,weights=NULL,total=!(se || ci),
      se=FALSE,ci=FALSE,ci.level=.95,
      total.name="N",perc.label="Percentage",\dots)
}
\arguments{
  \item{x}{a numeric vector or factor.}
  \item{weights}{a optional numeric vector of weights of the same length as \code{x}.}
  \item{total}{logical; should the total sum of counts from which the percentages are
    computed be included into the output?}
  \item{se}{logical; should standard errors of the percentages be included?}
  \item{ci}{logical; should confidence intervals of the percentages be included?}
  \item{ci.level}{numeric; nominal coverage of confidence intervals}
  \item{total.name}{character; name given for the total sum of counts}
  \item{perc.label}{character; label given for the percentages if the
    table has more than one dimensions, e.g. if \code{se} or \code{ci} is TRUE.}
  \item{\dots}{for \code{percent.mresp}: one or several 1-0 vectors or matrices
  otherwise, further arguments, currently ignored.}
}

\value{
  A table of percentages.
}

\examples{

x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
f <- sample(1:3,100,replace=TRUE)
f <- factor(f,labels=c("a","b","c"))


percent(x>0)
percent(f)

genTable(
  cbind(percent(x>0),
        percent(y>0),
        percent(z>0)) ~ f
  )

gt <- genTable(
  cbind(percent(x>0,ci=TRUE),
        percent(y>0,ci=TRUE),
        percent(z>0,ci=TRUE)) ~ f
  )

print(gt)
ftable(gt,row.vars=2,col.vars=c(3,1))

ex.data <- expand.grid(mean=c(0,25,50),sd=c(1,10,100))[rep(1:9,rep(250,9)),]
ex.data <- within(ex.data,x <- rnorm(n=nrow(ex.data),mean=ex.data$mean,sd=ex.data$sd))
ex.data <- within(ex.data,x.grp <- cases( x < 0,
                                            x >= 0 & x < 50,
                                            x >= 50 & x < 100,
                                            x >= 100
                                          ))
genTable(percent(x.grp)~mean+sd,data=ex.data)

aggregate(percent(Admit,weight=Freq)~Gender+Dept,data=UCBAdmissions)
}
\keyword{univar}
