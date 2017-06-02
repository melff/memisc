\name{mtable_format_delim}
\alias{mtable_format_delim}
\title{Format for 'mtable' Objects for Writing into File}
\description{
  \code{mtable_mtable_print} formats 'mtable' in a way suitable for output into a file
  with \code{write.table}
  }
\usage{
mtable_format_delim(x,
          colsep="\t",
          rowsep="\n",
          interaction.sep = " x ",
          \dots
          )
}
\arguments{
     \item{x}{an object of class \code{mtable}}
     \item{colsep}{a character string which seperates the columns in the output.}
     \item{rowsep}{a character string which seperates the rows in the output.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect}
    \item{\dots}{further arguments, ignored. }
}
\value{
    A character string.
}
\keyword{misc}
