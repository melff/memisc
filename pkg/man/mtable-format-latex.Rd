\name{mtable_format_latex}
\alias{mtable_format_latex}
\title{Format 'mtable' Results for LaTeX}
\description{
  This function formats objects created by \code{\link{mtable}} for inclusion
  into LaTeX files.
  }
\usage{
  mtable_format_latex(x,
            useDcolumn=getOption("useDcolumn",TRUE),
            colspec=if(useDcolumn) 
                      paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") 
                    else "l",
            LaTeXdec=".",
            ddigits=min(3,getOption("digits")),
            useBooktabs=getOption("useBooktabs",TRUE),
            toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
            midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
            cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
            bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
            interaction.sep = " $\\\\times$ ",
            sdigits=min(1,ddigits),
            compact=FALSE,
            sumry.multicol=FALSE,
            escape.tex=getOption("toLatex.escape.tex",FALSE),
            signif.notes.type=getOption("toLatex.signif.notes.type","include"),
            signif.notes.spec=getOption("toLatex.signif.notes.spec","p{.5\\\\linewidth}"),
            \dots
  )
}
\arguments{
     \item{x}{an object of class \code{mtable}}
     \item{useDcolumn}{should the \code{dcolumn} LaTeX package be used?
          If true, you will have to include \code{\\usepackage{dcolumn}} into
          the preamble of your LaTeX document.}
     \item{colspec}{LaTeX table column format specifyer(s).}
     \item{LaTeXdec}{the decimal point in the final LaTeX output.}
     \item{ddigits}{alignment specification or digits after the decimal point.}
     \item{useBooktabs}{should the \code{booktabs} LaTeX package be used?
          If true, you will have to include \code{\\usepackage{booktabs}} into
          the preamble of your LaTeX document.}
     \item{toprule}{appearance of the top border of the LaTeX \code{tabular} environment.}
     \item{midrule}{how are coefficients and summary statistics
        separated in the LaTeX \code{tabular} environment.}
     \item{cmidrule}{appearance of rules under section headings.}
     \item{bottomrule}{appearance of the bottom border of the LaTeX \code{tabular} environment.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect}
     \item{sdigits}{integer; number of digits after decimal dot for summary statistics.}
    \item{compact}{logical; should the table be compact, without extra columns
    between multi-equation models?}
    \item{sumry.multicol}{logical, should summaries enclosed into
      \code{\\multicol} commands?}
		\item{escape.tex}{logical, should symbols \code{$}, \code{_}, and \code{^} be
			escaped with backslashes?}
    \item{signif.notes.type}{character string; should be either
      \code{"include"},\code{"append"}, \code{"drop"},  or \code{"tnotes"}. If
      \code{"append"}, (very simple) LaTeX code is appended that contains
      notes that relate significance symbols to p-values. If
      \code{"include"}, the LaTeX table will include a (multi-column)
      cell with these notes. If \code{"drop"}, notes will not be added.
      If \code{"tnotes"}, the exported LaTeX
      table is wrapped in a \code{threeparttable} environment and the
      p-value notes are wrapped in a \code{tablenotes} environment. This
      requires the LaTeX package \code{threeparttable} in order to work.
    }
    \item{signif.notes.spec}{character string; specifies format
      of cells that include notes about p-values; relevant only if
      \code{signif.notes.type="include"}
    }
    \item{\dots}{further arguments, ignored. }
}
\value{
    A character string with code suitable for inclusion into a LaTeX-file.
}
\keyword{misc}
