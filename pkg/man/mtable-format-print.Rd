\name{mtable_format_print}
\alias{mtable_format_print}
\title{Print Format for 'mtable' Objects}
\description{
  \code{mtable_format_print} formats 'mtable' in a way suitable for screen output
  with 'print'.
  }
\usage{
mtable_format_print(x,
  topsep="=",
  bottomsep="=",
  sectionsep="-",
  interaction.sep = " x ",
  center.at=getOption("OutDec"),
  align.integers=c("dot","right","left"),
  padding = "  ",
  \dots
  )
}
\arguments{
     \item{x}{an object of class \code{mtable}}
     \item{topsep}{a character string that is recycled to a top rule.}
     \item{bottomsep}{a character string that is recycled to a bottom rule.}
     \item{sectionsep}{a character string that is recycled to seperate coefficients
        from summary statistics.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect}
     \item{center.at}{a character string on which resulting values are centered.
        Typically equal to ".". This is the default when \code{forLaTeX==TRUE}.
        If \code{NULL}, reported values are not centered.}
    \item{align.integers}{how to align integer values.}
    \item{padding}{a character string, usually whitespace, used to insert left- and right-padding of 
        table contents.}
    \item{\dots}{further arguments, ignored. }
}
\value{
    A character string.
}
\keyword{misc}
