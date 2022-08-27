\name{codebook}
\alias{codebook}
\alias{codebook,ANY-method}
\alias{codebook,item-method}
\alias{codebook,data.set-method}
\alias{codebook,importer-method}
\alias{codebook,NULL-method}
\alias{codebook,atomic-method}
\alias{codebook,data.frame-method}
\alias{codebook,tbl_df-method}
\alias{codebook,factor-method}
\alias{codebook-class}
\alias{format,codebookEntry-method}
\alias{show,codebook-method}
\alias{as.character,codebook-method}
\alias{$,codebook-method}
\alias{[,codebook,atomic,missing,ANY-method}
\alias{[[,codebook-method}
\title{Generate a Codebook of a Data Set}
\description{
  Function \code{codebook} collects documentation about an item,
  or the items in a data set or external data file. It returns
  an object that, when \code{show}n, print this documentation
  in a nicely formatted way. 
}
\usage{
codebook(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{item}(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{atomic}(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{factor}(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{data.set}(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{importer}(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{data.frame}(x, weights = NULL, unweighted = TRUE, \dots)
\S4method{codebook}{tbl_df}(x, weights = NULL, unweighted = TRUE, \dots)
}
\arguments{
  \item{x}{an \code{\link{item}}, numeric or character vector, factor,
    \code{\link{data.set}}, \code{\link{data.frame}} or \code{\link{importer}} object for \code{codebook()}}
  \item{weights}{an optional vector of weights.}
  \item{unweighted}{an optional logical vector; if weights are given, it
    determines of only summaries of weighted data are show or also summaries of
    unweighted data.}
  \item{\dots}{other arguments, currently ignored.}
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

codebook(Data)$vote
codebook(Data)[2]

codebook(Data[2])
 
DataFr <- as.data.frame(Data)
DataHv <- as_haven(Data,user_na=TRUE)

codebook(DataFr)
codebook(DataHv)

   
\dontrun{
Write(description(Data),
           file="Data-desc.txt")
Write(codebook(Data),
           file="Data-cdbk.txt")
  }
}
\keyword{manip}
