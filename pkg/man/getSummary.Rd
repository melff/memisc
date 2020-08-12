\name{getSummary}
\alias{getSummary}
\alias{getSummary_expcoef}
\alias{getSummary.lm}
\alias{getSummary.glm}
\alias{getSummary.clm}
\alias{getSummary.polr}
\alias{getSummary.simex}
\alias{getSummary_expcoef.default}
\alias{getSummary.aftreg}
\alias{getSummary.coxph}
\alias{getSummary.phreg}
\alias{getSummary.survreg}
\alias{getSummary.weibreg}
\alias{getSummary.merMod}
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
  \method{getSummary}{merMod}(obj, alpha=.05, \dots)

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

# These are contributed by Achim Zeileis
  \method{getSummary}{ivreg}(obj, alpha=.05,\dots)
  \method{getSummary}{tobit}(obj, alpha=.05,\dots)
  \method{getSummary}{hurdle}(obj, alpha=.05,\dots)
  \method{getSummary}{zeroinfl}(obj, alpha=.05,\dots)
  \method{getSummary}{betareg}(obj, alpha=.05,\dots)
  \method{getSummary}{multinom}(obj, alpha=.05,\dots)
  
# A variant that reports exponentiated coefficients.
# The default method calls 'getSummary()' internally and should
# be applicable to all classes for which 'getSummary()' methods exist.
getSummary_expcoef(obj, alpha=.05,\dots)
  \method{getSummary_expcoef}{default}(obj, alpha=.05,\dots)
}
\arguments{
  \item{obj}{a model object, e.g. of class \code{lm} or \code{glm}}
  \item{alpha}{level of the confidence intervals; their coverage should
    be 1-alpha/2 }
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
    The higher dimensions of the array correspond to
    the individual coefficients and, in multi-equation models,
    to the model equations.

    }
  \item{sumstat}{a vector containing the model summary statistics;
    the components may have arbitrary names.}
}


\keyword{misc}
\keyword{utilities}
