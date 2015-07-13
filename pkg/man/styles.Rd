\name{styles}
\alias{coef.style}
\alias{factor.style}
\alias{setCoefTemplate}
\alias{getCoefTemplate}
\alias{getSummaryTemplate}
\alias{setSummaryTemplate}
\alias{summaryTemplate}
\title{Formatting Styles for Coefficients, Factor Contrasts, and Summary Statistics}
\description{
  Methods for setting and getting templates for formatting
  model coefficients and summaries for use in \code{\link{mtable}}.
}
\usage{
setCoefTemplate(...)
getCoefTemplate(style)
getSummaryTemplate(x)
setSummaryTemplate(...)
summaryTemplate(x)
  }
\arguments{
  \item{...}{sevaral tagged arguments; in case of \code{setCoefTemplate} the
    tags specify the \code{coef.style}s, in case of \code{setSummaryTemplate}
    they specify model classes.
    The associated values are \link{template}s.}
  \item{style}{a character string with the name of a coefficient style,
      if left empty, all coefficient templates are returned.  }
  \item{x}{a model or a name of a model class, for example \code{"lm"} or \code{"glm"};
    if left empty, all summary templates are returned.}
}
\details{
  The style in which model coefficients are formatted by \code{\link{mtable}}
  is by default selected from the \code{coef.style} setting of \code{\link{options}},
  "factory-fresh" setting being \code{options(coef.style="default")}.

  The appearance of factor levels in an \code{\link{mtable}}
  can be influenced by the \code{factor.style} setting of \code{\link{options}}.
  The "factory-fresh" setting is \code{options(factor.style="($f): ($l)")},
  where \code{($f)} stands for the factor name and \code{($l)} stands
  for the factor level. In case of treatment contrasts, the baseline level
  will also appear in an \code{\link{mtable}} separated from the current
  factor level by the \code{baselevel.sep} setting of \code{\link{options}}.
  The "factory-fresh" setting is \code{options(baselevel.sep="-")},

  Users may specify additional coefficient styles by a call to \code{setCoefTemplate}.
  
  In order to adapt the display of summary statistics of other model classes, users need to
  set a template for model summaries via a call to \code{setSummaryTemplate}
  or to define a method of the generic function \code{summaryTemplate}.
}

\keyword{misc}
