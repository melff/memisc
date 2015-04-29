\name{mtable_format_html}
\alias{mtable_format_html}
\alias{format_html.mtable}
\title{HTML Formatting for 'mtable' Results}
\description{
  These functions formats 'mtable' objects into HTML format.
  }
\usage{
mtable_format_html(x,
                    interaction.sep = " &times; ",
                    toprule=2,midrule=1,bottomrule=2,
                    split.dec=TRUE,
                    \dots
  )
\method{format_html}{mtable}(x,
                    interaction.sep = " &times; ",
                    toprule=2,midrule=1,bottomrule=2,
                    split.dec=TRUE,
                    \dots
  )
}
\arguments{
     \item{x}{an object of class \code{mtable}}
     \item{toprule}{integer;
     thickness in pixels of rule at the top of the table.}
     \item{midrule}{integer;
     thickness in pixels of rules within the table.}
     \item{bottomrule}{integer;
     thickness in pixels of rule at the bottom of the table.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect}
    \item{split.dec}{logical; whether numbers should be centered
    at the decimal point by splitting the table cells.}
    \item{\dots}{further arguments, ignored. }
}
\value{
    A character string with code suitable for inclusion into a HTML-file.
}
\keyword{misc}
