\name{UnZip}
\alias{UnZip}
\title{Extract Files from Zip Files}
\description{
  \code{Unzip} extracts a file from a zip archive
  and puts them into a directory specified by
  the user or into the temporary directory of the
  current session.
}
\usage{
UnZip(zipfile,item,
  dir=tempdir(),package=NULL)
}
\arguments{
  \item{zipfile}{a character string, the path to the zip file.}
  \item{item}{a character string, full path (from the root
    of the zip file) to the file to extract.}
  \item{dir}{path to the directory were to place the extracted file.}
  \item{package}{optional package name, if given, the
    path to the zipfile starts in the packages' root directory.}
}
\examples{
# Extract American National Election Study of 1948
# It is item "NES1948.POR" in zip file "anes/NES1948.ZIP"
# where this path is relative to the packages'
# root directory.
nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR",
                     package="memisc")
nes1948.por
}