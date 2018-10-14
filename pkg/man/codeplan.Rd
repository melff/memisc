\name{codeplan}
\alias{codeplan}
\alias{codeplan,item.list-method}
\alias{codeplan<-}
\alias{setCodeplan}
\alias{setCodeplan,data.frame,codeplan-method}
\title{Describe structure of Data Sets and Importers}
\description{
The function \code{codeplan} creates a data frame that
  describes the structure of an item list (a \code{data.set} object or
  an \code{importer} object), so that this structure can be stored and
  and recovered. The resulting data frame has a particular print method
  that delimits the output to one line per variable.

  With \code{setCodeplan} can 
}
\usage{
codeplan(x)
\S4method{codeplan}{item.list}(x)
setCodeplan(x,value)
\S4method{setCodeplan}{data.frame,codeplan}(x,value)
codeplan(x) <- value
}
\arguments{
  \item{x}{for \code{codeplan(x)} an object that inherits from class \code{"item.list"},
  i.e. can be a \code{"data.set"} object or an \code{"importer"} object;
  for \code{codeplan(x) <- value} or \code{setCodeplan(x,value)} a data frame.}
  \item{value}{an object as it would be returned by \code{codeplan(x)}}
}
\value{\code{codeplan} returns a data frame with additional S3 class attribute \code{"codeplan"}.
Such a data frame has the following variables:
\itemize{
  \item \code{name}: The name of the item/variable in the item list or data set.
  \item \code{description}: The description/variable label string of the item/variable.
  \item \code{annotation}: code to recreate the annotation attribute,
  \item \code{labels}: code to recreate the value labels,
  \item \code{value.filter}: code to recreate the value filter attribute (declaration of
          missing values, range of valid values, or an enumeration of valid values.)
  \item \code{mode}: a character string that describes storage mode, such as \code{"character"},
        \code{"integer"}, or \code{"numeric"}.
  \item \code{measurement}: a character string with the measurement level,
        \code{"nominal"}, \code{"ordinal"}, \code{"interval"}, or \code{"ratio"}.
  }
}
\examples{
# Extract American National Election Study of 1948
nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())

# Get information about the variables contained.
nes1948 <- spss.portable.file(nes1948.por)

head(codeplan(nes1948))
}