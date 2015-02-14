\name{codebook}
\alias{codebook}
\alias{codebook,item-method}
\alias{codebook,data.set-method}
\alias{codebook,importer-method}
\alias{codebook-class}
\alias{format,codebookEntry-method}
\alias{show,codebook-method}
\alias{as.character,codebook-method}
\title{Generate a Codebook of a Data Set}
\description{
  Function \code{codebook} collects documentation about an item,
  or the items in a data set or external data file. It returns
  an object that, when \code{show}n, print this documentation
  in a nicely formatted way. 
}
\usage{
codebook(x)
\S4method{codebook}{data.set}(x)
\S4method{codebook}{importer}(x)
}
\arguments{
  \item{x}{an \code{\link{item}},  \code{\link{data.set}} or \code{\link{importer}} object for \code{codebook()}}
}
\value{
  An object of class "codebook", for which a \code{\link{show}} method exists that
  produces a nicely formatted output.
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
})

description(Data)

codebook(Data)

\dontrun{
Write(descrition(Data),
           file="Data-desc.txt")
Write(codebook(Data),
           file="Data-cdbk.txt")
  }
}
\keyword{manip}