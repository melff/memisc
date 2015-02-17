\name{importers}
\alias{importer}
\alias{importer-class}
\alias{spss.portable.file}
\alias{spss.portable.importer-class}
\alias{show,spss.portable.importer-method}
\alias{spss.system.file}
\alias{spss.system.importer-class}
\alias{show,spss.system.importer-method}
\alias{spss.fixed.file}
\alias{spss.fixed.importer-class}
\alias{show,spss.fixed.importer-method}
\alias{Stata.file}
\alias{Stata.importer-class}
\alias{show,Stata.importer-method}
\alias{as.data.set,importer-method}
\alias{subset,importer-method}
\alias{dim,importer-method}
\alias{names,importer-method}
\alias{[,importer,atomic,atomic,ANY-method}
\alias{[,importer,atomic,missing,ANY-method}
\alias{[,importer,missing,atomic,ANY-method}
\alias{[,importer,missing,missing,ANY-method}
\alias{initialize,spss.portable.importer-method}
\alias{initialize,spss.system.importer-method}
\alias{initialize,spss.fixed.importer-method}
\alias{initialize,Stata.importer-method}

\title{Object Oriented Interface to Foreign Files}
\description{
Importer objects are objects that refer to an external
data file. Currently only Stata files,
SPSS system, portable, and fixed-column files are supported.

Data are actually imported by `translating' an
importer file into a \code{\link{data.set}} using
\code{as.data.set} or \code{subset}.

The \code{importer} mechanism is more flexible and extensible
than \code{\link[foreign]{read.spss}} and  \code{\link[foreign]{read.dta}}
of package "foreign", as most of the parsing of the file headers is done in R.
It is also adapted to efficiently load large data sets.
Most importantly, importer objects support the
\code{\link{labels}}, \code{\link{missing.values}},
and \code{\link{description}}s, provided by this package.
}
\usage{
spss.fixed.file(file,
  columns.file,
  varlab.file=NULL,
  codes.file=NULL,
  missval.file=NULL,
  count.cases=TRUE,
  to.lower=TRUE
  )

spss.portable.file(file,
  varlab.file=NULL,
  codes.file=NULL,
  missval.file=NULL,
  count.cases=TRUE,
  to.lower=TRUE)

spss.system.file(file,
  varlab.file=NULL,
  codes.file=NULL,
  missval.file=NULL,
  count.cases=TRUE,
  to.lower=TRUE)

Stata.file(file)

## The most important methods for "importer" objects are:
\S4method{subset}{importer}(x, subset, select, drop = FALSE, \dots)

\S4method{as.data.set}{importer}(x,row.names=NULL,optional=NULL,
                    compress.storage.modes=FALSE,\dots)

}
\arguments{
    \item{x}{an object that inherits from class \code{"importer"}.}
    \item{file}{character string; the path to the file containing
        the data}
    \item{columns.file}{character string; the path to an
        SPSS/PSPP syntax file with a \code{DATA LIST FIXED} statement}
    \item{varlab.file}{character string; the path to an
        SPSS/PSPP syntax file with a \code{VARIABLE LABELS} statement}
    \item{codes.file}{character string; the path to an
        SPSS/PSPP syntax file with a \code{VALUE LABELS} statement}
    \item{missval.file}{character string; the path to an
        SPSS/PSPP syntax file with a \code{MISSING VALUES} statement}
    \item{count.cases}{logical; should cases in file be counted? This
      takes effect only if the data file does not already contain information
      about the number of cases.}
    \item{to.lower}{logical; should variable names changed to lower case?}
    \item{subset}{a logical vector or an expression containing variables
        from the external data file that evaluates to logical. }
    \item{select}{a vector of variable names from the external data file.
          This may also be a named vector, where the names give
          the names into which the variables from the external data
          file are renamed.}
    \item{drop}{a logical value, that determines what happens if
      only one column is selected. If TRUE and only one column
      is selected, \code{subset} returns only a single \code{item}
      object and not a \code{data.set}.}
    \item{row.names}{ignored, present only for compatibility.}
    \item{optional}{ignored, present only for compatibility.}
    \item{compress.storage.modes}{logical value; if TRUE floating point values
      are converted to integers if possible without loss of information.}
    \item{\dots}{other arguments; ignored.}
}
\value{
\code{spss.fixed.file}, \code{spss.portable.file},
\code{spss.system.file}, and \code{Stata.file}
return, respectively, objects of class
\code{"spss.fixed.importer"}, \code{"spss.portable.importer"},
\code{"spss.system.importer"}, or \code{"Stata.importer"},
which, by inheritance, are also objects of class \code{"importer"}.

Objects of class \code{"importer"} have at least the following two slots:

  \item{ptr}{an external pointer}
  \item{variables}{a list of objects of class \code{"item.vector"} which
    provides a `prototype' for the \code{"data.set"} set objects returned
    by the \code{as.data.set} and \code{subset} methods for objects of
    class \code{"importer"} }


The \code{as.data.frame} for \code{importer} objects does
the actual data import and returns a data frame. Note that in contrast
to \code{\link[foreign]{read.spss}}, the variable names of the
resulting data frame will be lower case, unless the importer function
is called with \code{to.lower=FALSE}. If long variable names
are defined (in case of a PSPP/SPSS system file), they take
precedence and are \emph{not} coerced to lower case.
}
\seealso{ \code{\link[memisc]{codebook}}, \code{\link[memisc]{description}},
\code{\link[foreign]{read.spss}}
}
\details{
    A call to a `constructor' for an importer object, that is,
    \code{spss.fixed.file}, \code{spss.portable.file}, \code{spss.sysntax.file},
    or \code{Stata.file},
    causes R to read in the header of the data file and/or
    the syntax files that contain information about
    the variables, such as the columns that they occupy
    (in case of \code{spss.fixed.file}), variable labels,
    value labels and missing values.

    The information in the file header and/or the accompagnying
    files is then processed to prepare the file for importing.
    Thus the inner structure of an \code{importer} object may
    well vary according to what type of file is to imported and
    what additional information is given.

    The \code{as.data.set} and \code{subset} methods
    for \code{"importer"} objects internally use the
    generic functions \code{seekData}, \code{readData},
    and \code{readSubset}, which have methods for the
    subclasses of \code{"importer"}.
    These functions are not callable
    from outside the package, however.
    
    Since the functions described here are more or less complete rewrite
    based on the description of the file structure provided
    by the documenation for PSPP, they are perhaps not as thorougly tested as the 
    functions in the \code{foreign} package, apart from the frequent use
    by the author of this package.
}
\examples{
# Extract American National Election Study of 1948
nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR",
                     package="memisc")

# Get information about the variables contained.
nes1948 <- spss.portable.file(nes1948.por)

# The data are not yet loaded:
show(nes1948)

# ... but one can see what variables are present:
description(nes1948)

# Now a subset of the data is loaded:
vote.socdem.48 <- subset(nes1948,
              select=c(
                  v480018,
                  v480029,
                  v480030,
                  v480045,
                  v480046,
                  v480047,
                  v480048,
                  v480049,
                  v480050
                  ))

# Let's make the names more descriptive:
vote.socdem.48 <- rename(vote.socdem.48,
                  v480018 = "vote",
                  v480029 = "occupation.hh",
                  v480030 = "unionized.hh",
                  v480045 = "gender",
                  v480046 = "race",
                  v480047 = "age",
                  v480048 = "education",
                  v480049 = "total.income",
                  v480050 = "religious.pref"
        )

# It is also possible to do both
# in one step:
# vote.socdem.48 <- subset(nes1948,
#              select=c(
#                  vote           = v480018,
#                  occupation.hh  = v480029,
#                  unionized.hh   = v480030,
#                  gender         = v480045,
#                  race           = v480046,
#                  age            = v480047,
#                  education      = v480048,
#                  total.income   = v480049,
#                  religious.pref = v480050
#                  ))



# We examine the data more closely:
codebook(vote.socdem.48)

# ... and conduct some analyses.
#
t(genTable(percent(vote)~occupation.hh,data=vote.socdem.48))

# We consider only the two main candidates.
vote.socdem.48 <- within(vote.socdem.48,{
  truman.dewey <- vote
  valid.values(truman.dewey) <- 1:2
  truman.dewey <- relabel(truman.dewey,
              "VOTED - FOR TRUMAN" = "Truman",
              "VOTED - FOR DEWEY"  = "Dewey")
  })

summary(truman.relig.glm <- glm((truman.dewey=="Truman")~religious.pref,
    data=vote.socdem.48,
    family="binomial",
))
}
\keyword{file}