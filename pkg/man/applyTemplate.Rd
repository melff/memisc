\name{applyTemplate}
\alias{applyTemplate}
\alias{template}
\title{Apply a Formatting Template to a Numeric or Character Vector}
\description{
  \code{applyTemplate} is called internally by \code{\link{mtable}}
  to format coefficients and summary statistics.
  }
\usage{
applyTemplate(x,template,float.style=getOption("float.style"),
                      digits=min(3,getOption("digits")),
                      signif.symbols=getOption("signif.symbols"))
}
\arguments{
 \item{x}{a numeric or character vector to be formatted}
 \item{template}{a character vector that defines the template, see details.}
 \item{float.style}{A character string that is passed to \code{\link{formatC}}
        by \code{applyTemplate}; valid values
        are \code{"e"}, \code{"f"}, \code{"g"}, \code{"fg"},
        \code{"E"}, and \code{"G"}. By default, the \code{float.style} 
        setting of \code{\link{options}} is used. The `factory fresh' setting is
        \code{options(float.style="f")}
        }
 \item{digits}{number of significant digits to use if not specified in
      the template.}
 \item{signif.symbols}{a named vector that specifies how significance levels
      are symbolically indicated, values of the vector specify
      significance levels and names specify the symbols. By default, the 
      \code{signif.symbols} setting of \code{\link{options}} is used. The "factory-fresh" setting is
        \code{options(signif.symbols=c("***"=.001,"**"=.01,"*"=.05))}.
      }
}
\details{
  Character vectors that are used as templates may be arbitrary. However,
  certain character sequences may form \emph{template expressions}.
  A template expression is of the form \code{($<POS>:<Format spec>)},
  where "\code{($}" indicates the start of a template expression,
  "\code{<POS>}" stands for either an index or name that selects an
  element from \code{x} and "\code{<Format spec>}" stands for a
  \emph{format specifier}. It may contain an letter indicating the
  style in which the vector element selected by \code{<POS>}
  will be formatted by \code{\link{formatC}}, it may contain
  a number as the number of significant digits, a "\code{#}"
  indicating that the number of signifcant digits will be at most that given
  by \code{getOption("digits")}, or \code{*} that means that
  the value will be formatted as a significance symbol.
}
\value{
  \code{applyTemplate} returns a character vector in which template
  expressions in \code{template} are substituted by formatted values from \code{x}.
  If \code{template} is an array then the return value is also an array of
  the same shape.
}
\examples{
applyTemplate(c(a=.0000000000000304,b=3),template=c("($1:g7#)($a:*)"," (($1:f2)) "))
applyTemplate(c(a=.0000000000000304,b=3),template=c("($a:g7#)($a:*)"," (($b:f2)) "))
}
\keyword{misc}
\keyword{utilities}
