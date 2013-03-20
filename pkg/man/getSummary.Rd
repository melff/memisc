\name{getSummary}
\alias{getSummary}
\alias{getSummary.expcoef}
\alias{getSummary.lm}
\alias{getSummary.glm}
\alias{getSummary.clm}
\alias{getSummary.polr}
\alias{getSummary.simex}
\alias{getSummary.expcoef}
\alias{getSummary.expcoef.glm}
\alias{getSummary.aftreg}
\alias{getSummary.coxph}
\alias{getSummary.phreg}
\alias{getSummary.survreg}
\alias{getSummary.weibreg}
\alias{getSummary.mer}
\alias{getSummary.ivreg}
\alias{getSummary.tobit}
\alias{getSummary.hurdle}
\alias{getSummary.zeroinfl}
\alias{getSummary.betareg}
\alias{getSummary.multinom}


\title{Get Model Summaries for Use with "mtable"}
\description{A generic function and methods to collect coefficients
  and summary statistics from a model object. It is used in \code{\link{mtable}}
  }
\usage{
  
  \method{getSummary}{lm}(obj, alpha=.05,\dots)
  \method{getSummary}{glm}(obj, alpha=.05,\dots)

  # These are contributed by Christopher N. Lawrence
  \method{getSummary}{clm}(obj, alpha=.05,\dots)
  \method{getSummary}{polr}(obj, alpha=.05,\dots)
  \method{getSummary}{simex}(obj, alpha=.05,\dots)

  # These are contributed by Jason W. Morgan
  \method{getSummary}{aftreg}(obj, alpha=.05,\dots)
  \method{getSummary}{coxph}(obj, alpha=.05,\dots)
  \method{getSummary}{phreg}(obj, alpha=.05,\dots)
  \method{getSummary}{survreg}(obj, alpha=.05,\dots)
  \method{getSummary}{weibreg}(obj, alpha=.05,\dots)
  \method{getSummary}{mer}(obj, alpha=.05, varPar.as.coef = TRUE, \dots)

  # These are contributed by Achim Zeileis
  \method{getSummary}{ivreg}(obj, alpha=.05,\dots)
  \method{getSummary}{tobit}(obj, alpha=.05,\dots)
  \method{getSummary}{hurdle}(obj, alpha=.05,\dots)
  \method{getSummary}{zeroinfl}(obj, alpha=.05,\dots)
  \method{getSummary}{betareg}(obj, alpha=.05,\dots)
  \method{getSummary}{multinom}(obj, alpha=.05,\dots)
  
  # These are contributed by Dave Atkins
  # Method for 'glm' objects - to report
  # exponentiated coefficients.
  \method{getSummary}{expcoef}(obj, alpha=.05,\dots)
  \method{getSummary.expcoef}{glm}(obj, alpha=.05,\dots)
}
\arguments{
  \item{obj}{a model object, e.g. of class \code{lm} or \code{glm}}
  \item{alpha}{level of the confidence intervals; their coverage should
    be 1-alpha/2 }
  \item{varPar.as.coef}{logical; should variance parameters included into the
    table of coefficients?}
  \item{\dots}{further arguments; ignored.}
}
\details{
  The generic function \code{getSummary} is called by \code{\link{mtable}}
  in order to obtain the coefficients and summaries of model objects.
  In order to adapt \code{\link{mtable}} to models of classes other
  than \code{\link{lm}} or \code{\link{glm}} one needs to
  define \code{getSummary} methods for these classes and
  to set a summary template via \code{\link{setSummaryTemplate}}
}
\value{
  Any method of \code{getSummary} must return a list with the following
  components:

  \item{coef}{an array with coefficient estimates;
    the lowest dimension \emph{must} have the following
    names and meanings:
    \tabular{rcl}{
    \code{est} \tab  \tab the coefficient estimates,\cr
    \code{se}  \tab  \tab the estimated standard errors,\cr
    \code{stat}\tab  \tab t- or Wald-z statistics,\cr
    \code{p}   \tab  \tab significance levels of the statistics,\cr
    \code{lwr} \tab  \tab lower confidence limits, \cr
    \code{upr} \tab  \tab upper confidence limits. \cr
    }
%     \describe{
%       \item{\code{est}}{the coefficient estimates,}
%       \item{\code{se}}{the estimated standard errors,}
%       \item{\code{stat}}{t- or Wald-z statistics,}
%       \item{\code{p}}{significance levels of the statistics,}
%       \item{\code{lwr}}{lower confidence limits,}
%       \item{\code{upr}}{upper confidence limits.}
%     }
    The higher dimensions of the array correspond to
    the individual coefficients and, in multi-equation models,
    to the model equations.

    }
  \item{sumstat}{a vector containing the model summary statistics;
    the components may have arbitrary names.}
}


\keyword{misc}
\keyword{utilities}