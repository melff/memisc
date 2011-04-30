\name{panel.errbars}
\alias{panel.errbars}
\title{Panel Functions for Error Bars}
\description{
\code{panel.errbars} plots points are draws a line suppelemented
  with error bars.
}
\usage{
panel.errbars(x,y,\dots,panel.xy=panel.xyplot,
                make.grid=c("horizontal","vertical","both","none"),ewidth=0)
}
\arguments{
  \item{x,y}{numeric values, the points around which error bars are plotted.
    \code{x} is a numeric vector, \code{y} is a matrix with three columns,
    the values, the lower and the upper ends of the error bars.
    }
  \item{\dots}{graphical parameters passed to \code{panel.xy}}
  \item{panel.xy}{panel function to produce the plot to which error bars are added}
  \item{make.grid}{character string, determines the direction of grid lines added to the plot}
  \item{ewidth}{numerical value, width of the whiskers of the error bars}
}
\seealso{\code{\link[lattice]{panel.xyplot}}}
\examples{
library(lattice)
library(grid)
\dontshow{
if(interactive())
  old.prompt <- grid::grid.prompt(TRUE)
}

applications <- aggregate(percent(Dept,weight=Freq,ci=TRUE)~Gender,
                                      data=UCBAdmissions)
admissions <- aggregate(
        percent(Admit=="Admitted",weight=Freq,ci=TRUE)~Dept+Gender,
                                      data=UCBAdmissions)
xyplot(cbind(Percentage,lower,upper)~Gender|Dept,data=admissions,
      panel=panel.errbars,
      ewidth=.2,pch=19,
      ylab="Percentage applicants admitted by Department")
xyplot(cbind(Percentage,lower,upper)~Gender|Dept,data=applications,
      panel=panel.errbars,
      ewidth=.2,pch=19,
      ylab="Percentage applications to the Department")

\dontshow{
if(interactive())
  grid::grid.prompt(old.prompt)
}

}
\keyword{hplot}