\name{mtable_format_latex}
\alias{mtable_format_latex}
\title{Format 'mtable' Results for LaTeX}
\description{
  This function formats objects created by \code{\link{mtable}} for inclusion
  into LaTeX files.
  }
\usage{
  mtable_format_latex(x,
            useDcolumn=TRUE,
            colspec=if(useDcolumn) 
                      paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") 
                    else "r",
            LaTeXdec=".",
            ddigits=min(3,getOption("digits")),
            useBooktabs=TRUE,
            toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
            midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
            cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
            bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
            interaction.sep = " $\\\\times$ ",
            sdigits=min(1,ddigits),
            force.names = FALSE,
            compact=FALSE,
            sumry.multicol=FALSE,
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
    \item{force.names}{logical; should model names be shown even
     if estimates of only one model are displayed?}
    \item{compact}{logical; should the table be compact, without extra columns
    between multi-equation models?}
    \item{sumry.multicol}{logical, should summaries enclosed into
         \code{\\multicol} commands?}
    \item{\dots}{further arguments, ignored. }
}
\value{
    A character string with code suitable for inclusion into a LaTeX-file.
}
\keyword{misc}
