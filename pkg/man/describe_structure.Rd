\name{describe_structure}
\alias{describe_structure}
\alias{describe_structure,item.list-method}
\title{Describe structure of Data Sets and Importers}
\description{
The function \code{decribe_structure} creates a data frame that
  describes the structure of an item list (a \code{data.set} object or
  an \code{importer} object), so that this structure can be stored and
  and recovered. The resulting data frame has a particular print method
  that delimits the output to one line per variable.
}
\usage{
describe_structure(x)
\S4method{describe_structure}{item.list}(x)
}
\arguments{
  \item{x}{an object that inherits from class \code{"item.list"},
  i.e. can be a \code{"data.set"} object or an \code{"importer"} object.}
}
\examples{
# Extract American National Election Study of 1948
nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())

# Get information about the variables contained.
nes1948 <- spss.portable.file(nes1948.por)

head(describe_structure(nes1948))
}