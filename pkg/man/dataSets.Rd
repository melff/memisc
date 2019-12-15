\name{data.set}
\alias{data.set}
\alias{data.set-class}
\alias{initialize,data.set-method}
\alias{is.data.set}
\alias{within,data.set-method}
\alias{print,data.set-method}
\alias{show,data.set-method}
\alias{summary,data.set-method}
\alias{as.data.frame.data.set}
\alias{str.data.set}
\alias{as.data.set}
\alias{as.data.set,list-method}
\alias{[<-,data.set-method}
\alias{[,data.set,atomic,atomic,ANY-method}
\alias{[,data.set,atomic,missing,ANY-method}
\alias{[,data.set,missing,atomic,ANY-method}
\alias{[,data.set,missing,missing,ANY-method}
\alias{dim,data.set-method}
\alias{dimnames,data.set-method}
\alias{dimnames<-,data.set-method}
\alias{row.names,data.set-method}
\alias{head,data.set-method}
\alias{tail,data.set-method}
\alias{tail,data.set-method}
\alias{as.data.table.data.set}
\alias{dsView}
\title{Data Set Objects}
\description{
  \code{"data.set"} objects are collections of \code{"item"} objects,
  with similar semantics as data frames. They are distinguished
  from data frames so that coercion by \code{as.data.fame}
  leads to a data frame that contains only vectors and factors.
  Nevertheless most methods for data frames are inherited by
  data sets, except for the method for the \code{within} generic
  function. For the \code{within} method for data sets, see the details section.

  Thus data preparation using data sets retains all informations
  about item annotations, labels, missing values etc.
  While (mostly automatic) conversion of data sets into data
  frames makes the data amenable for the use of R's statistical
  functions.
  
  \code{dsView} is a function that displays data sets in a similar
  manner as \code{View} displays data frames. (\code{View} works
  with data sets as well, but changes them first into data frames.)
}
\usage{
data.set(\dots,row.names = NULL, check.rows = FALSE, check.names = TRUE,
    stringsAsFactors = default.stringsAsFactors(),
                 document = NULL)
as.data.set(x, row.names=NULL, \dots)
\S4method{as.data.set}{list}(x,row.names=NULL,\dots)
is.data.set(x)
\method{as.data.frame}{data.set}(x, row.names = NULL, optional = FALSE, \dots)
\S4method{within}{data.set}(data, expr, \dots)

dsView(x)

\S4method{head}{data.set}(x,n=20,\dots)
\S4method{tail}{data.set}(x,n=20,\dots)
}
\arguments{
  \item{\dots}{
    For the \code{data.set} function several vectors or items,
    for \code{within} further, ignored arguments.
  }
  \item{row.names, check.rows, check.names, stringsAsFactors, optional}{arguments
    as in \code{\link[base]{data.frame}} or \code{\link[base]{as.data.frame}},
    respectively.
    }
  \item{document}{NULL or an optional character vector that contains
    documenation of the data.}
  \item{x}{for \code{is.data.set(x)}, any object; for
    \code{as.data.frame(x,...)} and \code{dsView(x)} a "data.set" object.}
  \item{data}{a data set, that is, an object of class "data.set". }
  \item{expr}{an expression, or several expressions enclosed in curly braces.}
  \item{n}{integer; the number of rows to be shown by \code{head} or \code{tail}}
}
\details{
  The \code{as.data.frame} method for data sets is just a copy
  of the method for list. Consequently, all items in the data set
  are coerced in accordance to their \code{\link{measurement}} setting,
  see \code{\link{item}} and \code{\link{measurement}}.

  The \code{within} method for data sets has the same effect as
  the \code{\link[base]{within}} method for data frames, apart from two differences:
  all results of the computations are coerced into items if
  they have the appropriate length, otherwise, they are automatically
  dropped.

  Currently only one method for the generic function \code{as.data.set}
  is defined: a method for "importer" objects.
}
\value{
\code{data.set} and the \code{within} method for
data sets returns a "data.set" object, \code{is.data.set}
returns a logical value, and \code{as.data.frame} returns
a data frame.
}
\examples{
Data <- data.set(
          vote = sample(c(1,2,3,8,9,97,99),size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),size=300,replace=TRUE),
          income = exp(rnorm(300,sd=.7))*2000
          )

Data <- within(Data,{
  description(vote) <- "Vote intention"
  description(region) <- "Region of residence"
  description(income) <- "Household income"
  wording(vote) <- "If a general election would take place next tuesday,
                    the candidate of which party would you vote for?"
  wording(income) <- "All things taken into account, how much do all
                    household members earn in sum?"
  foreach(x=c(vote,region),{
    measurement(x) <- "nominal"
    })
  measurement(income) <- "ratio"
  labels(vote) <- c(
                    Conservatives         =  1,
                    Labour                =  2,
                    "Liberal Democrats"   =  3,
                    "Don't know"          =  8,
                    "Answer refused"      =  9,
                    "Not applicable"      = 97,
                    "Not asked in survey" = 99)
  labels(region) <- c(
                    England               =  1,
                    Scotland              =  2,
                    Wales                 =  3,
                    "Not applicable"      = 97,
                    "Not asked in survey" = 99)
  foreach(x=c(vote,region,income),{
    annotation(x)["Remark"] <- "This is not a real survey item, of course ..."
    })
  missing.values(vote) <- c(8,9,97,99)
  missing.values(region) <- c(97,99)

  # These to variables do not appear in the
  # the resulting data set, since they have the wrong length.
  junk1 <- 1:5
  junk2 <- matrix(5,4,4)
  
})
# Since data sets may be huge, only a
# part of them are 'show'n
Data

\dontrun{

# If we insist on seeing all, we can use 'print' instead
print(Data)
}

str(Data)
summary(Data)

\dontrun{
# If we want to 'View' a data set we can use 'dsView'
dsView(Data)
# Works also, but changes the data set into a data frame first:
View(Data)
}

Data[[1]]
Data[1,]
head(as.data.frame(Data))

EnglandData <- subset(Data,region == "England")
EnglandData

xtabs(~vote+region,data=Data)
xtabs(~vote+region,data=within(Data, vote <- include.missings(vote)))
}
