\name{Iconv}
\alias{Iconv}
\alias{Iconv.annotation}
\alias{Iconv.data.set}
\alias{Iconv.importer}
\alias{Iconv.item}
\alias{Iconv.value.labels}

\title{Convert Annotations, and Value Labels between Encodings}
\description{
  This function uses the base package function \code{\link[base]{iconv}}
  to translate variable descriptions (a.k.a variable labels) and
  value labels of \code{\link{item}}, \code{\link{data.set}},
  and \code{\link{importer}} objects into a specified encoding.
  
  It will be useful in UTF-8 systems when data file come in some ancient
  encoding like 'Latin-1' as long used by Windows systems.
  }
\usage{

Iconv(x,from="",to="",\dots) 
\method{Iconv}{annotation}(x,from="",to="",\dots) 
\method{Iconv}{data.set}(x,from="",to="",\dots) 
\method{Iconv}{importer}(x,from="",to="",\dots) 
\method{Iconv}{item}(x,from="",to="",\dots) 
\method{Iconv}{value.labels}(x,from="",to="",\dots) 

}
\arguments{
  \item{x}{an object of which attributes are to be re-encoded.}
  \item{from}{a character string desribing the original encoding}
  \item{to}{a character string desribing the target encoding}
  \item{\dots}{further arguments, passed to \code{\link[base]{iconv}}}
}
\value{
  Iconv returns a copy of its first argument with re-encoded 
  attributes.
}
\seealso{
  \code{\link[base]{iconv}}, \code{\link[base:iconv]{iconvlist}}
}
\examples{
\dontrun{
# Locate an SPSS 'system' file and get info on variables, their labels etc.
ZA5302 <- spss.system.file("Daten/ZA5302_v6-0-0.sav",to.lower=FALSE)

# Convert labels etc. from 'latin1' to the encoding of the current locale.
ZA5302 <- Iconv(ZA5302,from="latin1")

# Write out the codebook
writeLines(as.character(codebook(ZA5302)),
           con="ZA5302-cdbk.txt")

# Write out the description of the varialbes (their 'variable labels')
writeLines(as.character(description(ZA5302)),
            con="ZA5302-description.txt")
}
}
\keyword{misc}
