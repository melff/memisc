\name{ftable-matrix}
\alias{ftable_matrix}
\alias{cbind.ftable}
\alias{rbind.ftable}
\alias{cbind.ftable_matrix}
\alias{rbind.ftable_matrix}
\alias{format.ftable_matrix}
\alias{print.ftable_matrix}
\alias{Write.ftable_matrix}

\title{
 Combining flattened tables.
}
\description{
 With the method functions described here, flattened (contingency) tables can be combined
 into more complex objects, of class \code{"ftable_matrix"}. For objects of these class 
 \code{format} and \code{print} methods are provided
}
\usage{
\method{cbind}{ftable}(\dots, deparse.level=1)

\method{rbind}{ftable}(\dots, deparse.level=1)

\method{cbind}{ftable_matrix}(\dots, deparse.level=1)

\method{rbind}{ftable_matrix}(\dots, deparse.level=1)

\method{format}{ftable_matrix}(x,quote=TRUE,digits=0,format="f",\dots)

\method{Write}{ftable_matrix}(x,
                            file = "",
                            quote = TRUE,
                            append = FALSE,
                            digits = 0,
                            \dots)
                            
\method{print}{ftable_matrix}(x,quote=FALSE,\dots)
}
\arguments{
  \item{\dots}{for \code{cbind} and \code{rbind} methods, two or more objects
  of class \code{"ftable"} or \code{"ftable_matrix"}; for the other methods: further 
  arguments, ignored.}
  \item{deparse.level}{ignored, retained for compatibility reasons only.}
  \item{x}{an object used to select a method.}
  \item{quote}{logical, indicating whether or not strings should be printed with 
        surrounding quotes.} 
  \item{digits}{numeric or integer, number of significant digits to be shown.}
  \item{format}{a format string as in \code{\link{formatC}}} 
  \item{file}{character string, containing a file path.}
  \item{append}{logical, should the output appended to the file?}
}
\value{
  \code{cbind} and \code{rbind}, when used with \code{"ftable"} or \code{"ftable_matrix"}
  objects, return objects of class \code{"ftable_matrix"}.
}
\examples{
ft1 <- ftable(Sex~Survived,Titanic)
ft2 <- ftable(Age+Class~Survived,Titanic)
ft3 <- ftable(Survived~Class,Titanic)
ft4 <- ftable(Survived~Age,Titanic)
ft5 <- ftable(Survived~Sex,Titanic)

tab10 <- xtabs(Freq~Survived,Titanic)

(c12.10 <- cbind(ft1,ft2,Total=tab10))
(r345.10 <- rbind(ft3,ft4,ft5,Total=tab10))

\dontrun{
tf <- tempfile()
Write(c12.10,file=tf)
file.show(tf)
}
}
