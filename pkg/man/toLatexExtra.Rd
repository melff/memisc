\name{toLatex}
\alias{toLatex.default}
\alias{toLatex.ftable}
\alias{toLatex.matrix}
\title{Additional Methods for LaTeX Representations for R objects}
\description{
  Methods for the generic function \code{\link[utils]{toLatex}} of package ``\link{utils}''
  are provided for generating LaTeX representations
  of matrices and flat contingency tables (see \code{\link{ftable}}). Also a default method is defined
  that coerces its first argument into a matrix and applies
  the matrix method.
}
\usage{
\method{toLatex}{default}(object,\dots)
\method{toLatex}{ftable}(object,
    show.titles=TRUE,
    digits=0,
    format="f",
    useDcolumn=TRUE,
    colspec= if (useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=TRUE,
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline\n",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    extrarowsep = NULL,
    \dots)
\method{toLatex}{matrix}(object,
    show.titles=TRUE,
    show.vars=FALSE,
    show.xvar=show.vars,
    show.yvar=show.vars,
    digits=if(is.table(object)) 0 else getOption("digits"),
    format="f",
    useDcolumn=TRUE,
    colspec= if (useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=TRUE,
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    \dots)
}
\arguments{
     \item{object}{an \code{\link{ftable}}, a matrix or an object coercable into a matrix.  }
     \item{show.titles}{logical; should variable names (in case of the \code{ftable} and \code{table} methods)
      or row and column names (in case of the \code{matrix} method) be appear
      in the \code{LaTeX} code? }
     \item{show.vars,show.xvar,show.yvar}{logical; should the names of the dimnames of \code{object}
      be shown in the margins of the LaTeX tabular? Such names usually represent the
      row and/or column variables of a two-dimensional \code{\link{table}}.
     }
     \item{digits}{number of significant digits.
        }
     \item{format}{format specifier, see \code{\link{format}}. }
     \item{useDcolumn}{should the facilities of the \code{dcolumn} LaTeX package be used?
          Note that, if TRUE, you will need to include \code{\\usepackage{dcolumn}}
          in the preamble of your LaTeX document.}
     \item{colspec}{LaTeX table column format specifyer(s).}
     \item{LaTeXdec}{the decimal point in the final LaTeX output.}
     \item{ddigits}{alignment specification or digits after the decimal point.}
     \item{useBooktabs}{should the facilities of the \code{booktabs} LaTeX package be used?
          Note that, if TRUE, you will need to include \code{\\usepackage{booktabs}}
          in the preamble of your LaTeX document.}
     \item{toprule}{appearance of the top border of the LaTeX \code{tabular} environment.}
     \item{midrule}{how are coefficients and summary statistics
        separated in the LaTeX \code{tabular} environment.}
     \item{cmidrule}{appearance of rules under section headings.}
     \item{bottomrule}{appearance of the bottom border of the LaTeX \code{tabular} environment.}
     \item{extrarowsep}{extra code to be inserted between the column titles and the
     table body produced by \code{toLatex}. }
     \item{\dots}{further argument, currently ignored.}
}
\examples{
toLatex(diag(5))
toLatex(ftable(UCBAdmissions))
}
\keyword{misc}