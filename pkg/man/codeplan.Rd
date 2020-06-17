\name{codeplan}
\alias{codeplan}
\alias{codeplan,ANY-method}
\alias{codeplan,item.list-method}
\alias{codeplan,item-method}
\alias{codeplan<-}
\alias{setCodeplan}
\alias{setCodeplan,data.frame,codeplan-method}
\alias{setCodeplan,data.set,codeplan-method}
\alias{setCodeplan,data.set,NULL-method}
\alias{setCodeplan,item,codeplan-method}
\alias{setCodeplan,item,NULL-method}
\alias{setCodeplan,atomic,codeplan-method}
\alias{setCodeplan,atomic,NULL-method}
\title{Describe structure of Data Sets and Importers}
\description{
  The function \code{codeplan()} creates a data frame that
  describes the structure of an item list (a \code{data.set} object or
  an \code{importer} object), so that this structure can be stored and
  and recovered. The resulting data frame has a particular print method
  that delimits the output to one line per variable.

  With \code{setCodeplan} an item list structure (as returned by \code{codeplan()})
  can be applied to a data frame or data set. It is also possible to use an
  assignment like \code{codeplan(x) <- value} to a similar effect.
}
\usage{
codeplan(x)
\S4method{codeplan}{item.list}(x)
\S4method{codeplan}{item}(x)
setCodeplan(x,value)
\S4method{setCodeplan}{data.frame,codeplan}(x,value)
\S4method{setCodeplan}{data.set,codeplan}(x,value)
\S4method{setCodeplan}{data.set,NULL}(x,value)
\S4method{setCodeplan}{item,codeplan}(x,value)
\S4method{setCodeplan}{item,NULL}(x,value)
\S4method{setCodeplan}{atomic,codeplan}(x,value)
\S4method{setCodeplan}{atomic,NULL}(x,value)
codeplan(x) <- value
}
\arguments{
  \item{x}{for \code{codeplan(x)} an object that inherits from class \code{"item.list"},
    i.e. can be a \code{"data.set"} object or an \code{"importer"}
  object, it can also be an object that inherits from class \code{"item"}}
  \item{value}{an object as it would be returned by \code{codeplan(x)}
  or \code{NULL}.}
}
\value{If applicable, \code{codeplan} returns a data frame with
  additional S3 class attribute \code{"codeplan"}. For arguments for
  which the relevant information does not exist, the function returns \code{NULL}.
Such a data frame has the following variables:
  \item{\code{name}}{The name of the item/variable in the item list or data set.}
  \item{\code{description}}{The description/variable label string of the item/variable.}
  \item{\code{annotation}}{code to recreate the annotation attribute,}
  \item{\code{labels}}{code to recreate the value labels,}
  \item{\code{value.filter}}{code to recreate the value filter attribute (declaration of
          missing values, range of valid values, or an enumeration of valid values.)}
  \item{\code{mode}}{a character string that describes storage mode, such as \code{"character"},
        \code{"integer"}, or \code{"numeric"}.}
  \item{\code{measurement}}{a character string with the measurement level,
        \code{"nominal"}, \code{"ordinal"}, \code{"interval"}, or
        \code{"ratio"}.}
  If \code{codeplan(x)<-value} or \code{setCodeplan(x,value)} is used
  and \code{value} is \code{NULL}, all the special information about
  annotation, labels, value filters, etc. is removed from the resulting
  object, which then is usually a mere atomic vector or data frame.
}
\examples{
Data1 <- data.set(
          vote = sample(c(1,2,3,8,9,97,99),size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),size=300,replace=TRUE),
          income = exp(rnorm(300,sd=.7))*2000
          )

Data1 <- within(Data1,{
  description(vote) <- "Vote intention"
  description(region) <- "Region of residence"
  description(income) <- "Household income"
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
cpData1 <- codeplan(Data1)

Data2 <- data.frame(
          vote = sample(c(1,2,3,8,9,97,99),size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),size=300,replace=TRUE),
          income = exp(rnorm(300,sd=.7))*2000
          )
codeplan(Data2) <- cpData1
codebook(Data2)

# Note the difference between 'as.data.frame' and setting
# the codeplan to NULL:
Data2df <- as.data.frame(Data2)
codeplan(Data2) <- NULL
str(Data2)
str(Data2df)

# Codeplans of survey items can also be inquired and manipulated:
vote <- Data1$vote
str(vote)
cp.vote <- codeplan(vote)
codeplan(vote) <- NULL
str(vote)
codeplan(vote) <- cp.vote
vote
}
