\name{getSummary}
\alias{getSummary}
\alias{getSummary.lm}
\alias{getSummary.glm}
\alias{getSummary.polr}
\alias{getSummary.simex}
\alias{getSummary.lmer}
\alias{getSummary.mer}
\alias{getSummary.expcoef.glm}
\alias{getSummary.clm}


\title{Get Model Summaries for Use with "mtable"}
\description{A generic function and methods to collect coefficients
  and summary statistics from a model object. It is used in \code{\link{mtable}}
  }
\usage{
  \method{getSummary}{lm}(obj, alpha=.05,\dots)
  \method{getSummary}{glm}(obj, alpha=.05,\dots)
  \method{getSummary}{polr}(obj, alpha=.05,\dots)
  \method{getSummary}{simex}(obj, alpha=.05,\dots)
  \method{getSummary}{lmer}(obj, alpha=.05,\dots)
  \method{getSummary}{mer}(obj, alpha=.05,\dots)
  \method{getSummary}{clm}(obj, alpha=.05,\dots)

  # Method for 'glm' objects - to report
  # exponentiated coefficients.
  getSummary.expcoef.glm(obj, alpha=.05,\dots)
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
\note{
  \code{getSummary.polr} and \code{getSummary.simex} are contributions
  from Christopher N. Lawrence (Texas A&M International University).
  \code{getSummary.lmer} and \code{getSummary.expcoef.glm} are contributions
  from Dave Atkins (Unversity of Washington).
}

\keyword{misc}
\keyword{utilities}