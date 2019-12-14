\name{Write}
\alias{Write}
\alias{Write.codebook}
\alias{Write.descriptions}

\title{Write Codebooks and Variable Descriptions into a Text File}
\description{
   This is a convenience function to facilitate the creation of data set documents
   in text files.
  }
\usage{
Write(x,\dots)
\S3method{Write}{codebook}(x,file=stdout(),\dots)
\S3method{Write}{descriptions}(x,file=stdout(),\dots)
}
\arguments{
  \item{x}{a "codebook" or "descriptions" object.}
  \item{file}{ a connection, see \link{connections}.}
  \item{\dots}{ further arguments, ignored or passed on to particular methods.}
}
\keyword{misc}
