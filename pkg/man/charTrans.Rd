\name{charTrans}
\alias{charTrans}
\alias{charTrans.character}
\alias{charTrans.annotation}
\alias{charTrans.data.set}
\alias{charTrans.importer}
\alias{charTrans.item}
\alias{charTrans.value.labels}
\encoding{UTF-8}

\title{Character Translation of Aspects of Objects}
\description{
  This function uses the base package function \code{\link[base]{chartr}}
  to translate characters in variable descriptions (a.k.a variable labels) and
  value labels of \code{\link{item}}, \code{\link{data.set}},
  and \code{\link{importer}} objects.
  
  It will be useful when the encoding of an important data set 
  cannot be fully identified or if the encoding in the data file
  is incorrect or unknown.
}
\usage{

charTrans(x, old = "", new = "", \dots)
\method{charTrans}{annotation}(x, old = "", new = "", \dots)
\method{charTrans}{data.set}(x, old = "", new = "", \dots)
\method{charTrans}{importer}(x, old = "", new = "", \dots)
\method{charTrans}{item}(x, old = "", new = "", \dots)
\method{charTrans}{value.labels}(x, old = "", new = "", \dots)

}
\arguments{
  \item{x}{a character vector or an object of which character data or attributes
   character-translated.}
  \item{old}{a string with the characters to be translated.}
  \item{new}{a string with the translated characters.}
  \item{\dots}{further arguments, currently ignored.}
}
\value{
  \code{charTrans} returns a copy of its first argument with character-translated
  character data or attributes.
}
\seealso{
  \code{\link[base]{chartr}}
}
\examples{
\dontrun{
# Locate an SPSS 'portable' file and get info on variables, their labels 
S2601.POR <- spss.portable.file("POR-Files/S2601.POR", encoded = "cp850")
# 'ß' appears to be correctly coded, but 'ä', 'ö', 'ü' are not, so we need to
# to some fine-tuning
S2601.POR <- charTrans(S2601.POR, old="{|}\r", new="äöü ")
# Now labels etc. are correctly encoded.
codebook(S2601.POR)
}
}