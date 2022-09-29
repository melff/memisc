\name{toLatex}
\alias{toLatex.default}
\alias{toLatex.ftable}
\alias{toLatex.matrix}
\alias{toLatex.data.frame}
\alias{toLatex.ftable_matrix}
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

\method{toLatex}{matrix}(object,
    show.titles=TRUE,
    show.vars=FALSE,
    show.xvar=show.vars,
    show.yvar=show.vars,
    digits=if(is.table(object)) 0 else getOption("digits"),
    format="f",
    useDcolumn=getOption("useDcolumn",TRUE),
    colspec=if(useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    \dots)


\method{toLatex}{data.frame}(object,
    digits=getOption("digits"),
    format="f",
    useDcolumn=getOption("useDcolumn",TRUE),
    numeric.colspec=if(useDcolumn)
                       paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
                    else "r",
    factor.colspec="l",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    row.names=is.character(attr(object,"row.names")),
    NAas="",
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    \dots)

\method{toLatex}{ftable}(object,
    show.titles=TRUE,
    digits=if(is.integer(object)) 0 else getOption("digits"),
    format=if(is.integer(object)) "d" else "f",
    useDcolumn=getOption("useDcolumn",TRUE),
    colspec=if(useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline\n",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    extrarowsep = NULL,
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    fold.leaders=FALSE,
    \dots)

\method{toLatex}{ftable_matrix}(object,
    show.titles=TRUE,
    digits=getOption("digits"),
    format="f",
    useDcolumn=getOption("useDcolumn",TRUE),
    colspec=if(useDcolumn)
                paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
             else "r",
    LaTeXdec=".",
    ddigits=digits,
    useBooktabs=getOption("useBooktabs",TRUE),
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    compact=FALSE,
    varontop,varinfront,
    groupsep="3pt",
    grouprule=midrule,
    toLatex.escape.tex=getOption("toLatex.escape.tex",FALSE),
    \dots)
}
\arguments{
     \item{object}{an \code{\link{ftable}}, a matrix or an object coercable into 
     a matrix.  }
     \item{show.titles}{logical, should variable names (in case of the 
     \code{ftable} and \code{table} methods)
      or row and column names (in case of the \code{matrix} method) be appear
      in the \code{LaTeX} code? }
     \item{show.vars,show.xvar,show.yvar}{logical, should the names of the dimnames of \code{object}
      be shown in the margins of the LaTeX tabular? Such names usually represent the
      row and/or column variables of a two-dimensional \code{\link{table}}.
     }
     \item{digits}{number of significant digits.
        }
     \item{format}{character containing a format specifier, see \code{\link{format}}. }
     \item{useDcolumn}{logical, should the facilities of the \code{dcolumn} LaTeX package be used?
          Note that, if TRUE, you will need to include \code{\\usepackage{dcolumn}}
          in the preamble of your LaTeX document.}
     \item{colspec}{character, LaTeX table column format specifyer(s).}
     \item{numeric.colspec}{character, LaTeX table column format
          specifyer(s) for numeric vectors in the data frame.}
     \item{factor.colspec}{character, LaTeX table column format
          specifyer(s) for factors in the data frame.}
     \item{LaTeXdec}{character, the decimal point in the final LaTeX output.}
     \item{ddigits}{integer, digits after the decimal point.}
     \item{useBooktabs}{logical, should the facilities of the \code{booktabs} LaTeX package be used?
          Note that, if TRUE, you will need to include \code{\\usepackage{booktabs}}
          in the preamble of your LaTeX document.}
     \item{toprule}{character string, TeX code that determines the appearance of the top border of the LaTeX \code{tabular} environment.}
     \item{midrule}{character string, TeX code that determines how  coefficients and summary statistics are
        separated in the LaTeX \code{tabular} environment.}
     \item{cmidrule}{character string, TeX code that determines the appearance of rules under section headings.}
     \item{bottomrule}{character string, TeX code that determines the appearance of the bottom border of the LaTeX \code{tabular} environment.}
     \item{extrarowsep}{character string, extra code to be inserted between the column titles and the
     table body produced by \code{toLatex}. }
     \item{compact}{logical, if \code{TRUE}, extra column space between sub-tables 
        is suppressed. Defaults to \code{FALSE}}
     \item{varontop}{logical, whether names of column variables should appear on top of factor levels}
     \item{varinfront}{logical, whether names of row variables should appear in front of factor levels}
     \item{groupsep}{character string, containing a TeX length; extra 
      vertical space inserted between sub-tables, unless \code{compact} is \code{TRUE}.}
     \item{grouprule}{character string, TeX code that determines how
       sub-table headings are embellished.}
     \item{row.names}{logical, whether row names should be included in
			 exported LaTeX code.}
     \item{NAas}{character string, how missing values should be represented.}
     \item{toLatex.escape.tex}{logical, should symbols "$", "_", and "^" be
	   escaped with backslashes?}
     \item{fold.leaders}{logical, if \code{TRUE}, factor levels of row
       variables are not distributed into different columns, but
       'folded' into a single column.}
     \item{\dots}{further argument, currently ignored.}
}
\examples{
toLatex(diag(5))

toLatex(ftable(UCBAdmissions))

toLatex(rbind(
  ftable(margin.table(UCBAdmissions,c(2,1))),
  ftable(margin.table(UCBAdmissions,c(3,1)))
))
}
\keyword{misc}
