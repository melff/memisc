\name{Termplot}
\alias{Termplot}
\alias{Termplot.default}
\alias{Termplot.lmList}
\title{Produce a Term Plot Lattice}
\description{
  \code{Termplot} produces a lattice plot of termplots.
  Terms are not plotted individually, rather the terms
  in which a variable appears are summed and plotted.
  }
\usage{
  
\method{Termplot}{default}(object,
        ...,
        variables=NULL,
        col.term = 2,
        lty.term = 1,
        lwd.term = 1.5,
        se = TRUE,
        col.se = "orange",
        lty.se = 2,
        lwd.se = 1,
        col.res = "gray",
        residuals = c("deviance","none","pearson","working"),
        cex = 1,
        pch = 1,
        jitter.resid=FALSE,
        smooth = TRUE,
        col.smth = "darkred",
        lty.smth = 2,
        lwd.smth = 1,
        span.smth = 2/3,
        aspect="fill",
        xlab=NULL,
        ylab=NULL,
        main=paste(deparse(object$call),collapse="\n"),
        models=c("rows","columns"),
        xrot = 0,
        layout=NULL)
}
\arguments{
  \item{object}{an model fit object, or a list of model objects}
  \item{...}{further model objects.}
  \item{variables}{a character vector giving the names of 
    independent variables; note that
    the combined effect of all terms containing the respective
    values will be plotted; if empty, the effect of each independent
    variable will be plotted. Currently, higher-order terms
    will be ignored.}
  \item{col.term,lty.term,lwd.term}{parameters for the graphical representation
    of the terms with the same meaning as in \code{\link{termplot}}.}
  \item{se}{a logical value; should standard error curves added to the plot?}
  \item{col.se,lty.se,lwd.se}{graphical parameters for the
    depiction of the standard error curves, see \code{\link{termplot}}.}
  \item{residuals}{a character string, to select the type of
    residuals added to the plot.}
  \item{col.res,cex,pch}{graphical parameters for the depiction
        the residuals as in \code{\link{termplot}}.}
  \item{jitter.resid}{a logical vector of at most length 2.
      Determines whether residuals should be jittered
      along the x-axis (first element) and along the y-axis.
      If this argument has length 1, its setting applies to
      both axes.}
  \item{smooth}{a logical value; should a LOWESS smooth added to the plot?}
  \item{span.smth}{a numerical value, the span of the smoother.}
  \item{col.smth,lty.smth,lwd.smth}{graphical parameters for the smoother curve.}
  \item{aspect}{aspect ratio of the plot(s), see \code{\link[lattice]{xyplot}}.}
  \item{xlab}{label of the x axis, see \code{\link[lattice]{xyplot}}.}
  \item{ylab}{label of the y axis, see \code{\link[lattice]{xyplot}}.}
  \item{main}{main heading, see \code{\link[lattice]{xyplot}}.}
  \item{models}{character; should models arranged in rows or columns?}
  \item{xrot}{angle by which labels along the x-axis are rotated.}
  \item{layout}{layout specification, see \code{\link[lattice]{xyplot}.}}
}
\value{
  A trellis object.
}
\examples{

library(lattice)
library(grid)
\dontshow{
if(interactive())
  old.prompt <- devAskNewPage(TRUE)
}

lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

Termplot(lm2)
Termplot(berk2)
Termplot(lm0,lm1,lm2)
Termplot(berk0,berk1,berk2)

Termplot(By(~Gender,glm(cbind(Admitted,Rejected)~Dept,family="binomial"),
                    data=berkeley))
Termplot(By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                    data=berkeley))

require(splines)
xyz <- data.frame(
  x = 1:100,
  z = factor(rep(LETTERS[1:4],25))
)
xyz <- within(xyz,
  y <- rnorm(100,sin(x/10)+x/50+as.numeric(z))
)
yxz.lin <- glm(y ~ x + z, data=xyz)
yxz.bs <- glm(y ~ bs(x,6) + z, data=xyz)
yxz.ns <- glm(y ~ ns(x,6) + z, data=xyz)
yxz.poly <- glm(y ~ poly(x,6) + z, data=xyz)
yxz.sincos <- glm(y ~ sin(x/10) + cos(x/10) + x + z, data=xyz)

# Terms containing
# the same variable are not plotted
# individually but their combined effect is plotted
#
Termplot(yxz.lin,yxz.bs,yxz.ns,yxz.poly,yxz.sincos,models="columns",
  span.smth=1/3)

Termplot(yxz.lin,yxz.bs,yxz.ns,yxz.poly,yxz.sincos,variables="x",
  span.smth=1/3)


\dontshow{
if(interactive())
  devAskNewPage(old.prompt)
}
  
}
\keyword{hplot}
