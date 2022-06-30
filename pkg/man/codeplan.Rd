\name{codeplan}
\alias{codeplan}
\alias{codeplan,ANY-method}
\alias{codeplan,item.list-method}
\alias{codeplan,item-method}
\alias{codeplan<-}
\alias{setCodeplan}
\alias{setCodeplan,data.frame,codeplan-method}
\alias{setCodeplan,data.frame,NULL-method}
\alias{setCodeplan,data.set,codeplan-method}
\alias{setCodeplan,data.set,NULL-method}
\alias{setCodeplan,item,codeplan-method}
\alias{setCodeplan,item,NULL-method}
\alias{setCodeplan,atomic,codeplan-method}
\alias{setCodeplan,atomic,NULL-method}
\alias{read_codeplan}
\alias{write_codeplan}
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
\S4method{setCodeplan}{data.frame,NULL}(x,value)
\S4method{setCodeplan}{data.set,codeplan}(x,value)
\S4method{setCodeplan}{data.set,NULL}(x,value)
\S4method{setCodeplan}{item,codeplan}(x,value)
\S4method{setCodeplan}{item,NULL}(x,value)
\S4method{setCodeplan}{atomic,codeplan}(x,value)
\S4method{setCodeplan}{atomic,NULL}(x,value)
codeplan(x) <- value
read_codeplan(filename,type)
write_codeplan(x,filename,type,pretty)
}
\arguments{
  \item{x}{for \code{codeplan(x)} an object that inherits from class \code{"item.list"},
    i.e. can be a \code{"data.set"} object or an \code{"importer"}
  object, it can also be an object that inherits from class
  \code{"item"}.
   For \code{write_codeplan} an object from class \code{"codeplan"}.}
  \item{value}{an object as it would be returned by \code{codeplan(x)}
    or \code{NULL}.}
  \item{filename}{a character string, the name of the file that is to be
                  read or to be written.}
  \item{type}{a character string (either "yaml" or "json") oder NULL (the default), gives the type
              of the file into which the codeplan is written or from
              which it is read.
              If \code{type} is NULL then the file type is inferred from
              the file name ending (".yaml" or ",yml" for "yaml",
              ".json" for "json").
            }
  \item{pretty}{a logical value, whether the JSON output created by \code{write_codeplan(...)} should be prettified.}
}
\value{If applicable, \code{codeplan} returns a list with
  additional S3 class attribute \code{"codeplan"}. For arguments for
  which the relevant information does not exist, the function returns \code{NULL}.

  The list has at least one element or several elements, named after the
  variable in the "item.list" or "data.set" \code{x}. Each list element
  is a list itself with the following elements:

  \item{\code{annotation}}{a named character vector,}
  \item{\code{labels}}{a named list of labels and labelled values}
  \item{\code{value.filter}}{a list with at least two elements named
      "class" and "filter", and optionally another element named
      "range". The "class" element determines the class of the
      value filter and equals either "missing.values", "valid.values",
      or "valid.range". An element named "range" may only be needed
      if "class" is "missing.values", as it is possible (like in SPSS)
      to have \emph{both} individual missing values and a range of
      missing values.
  }
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
codeplan(Data2)
codebook(Data2)

# Note the difference between 'as.data.frame' and setting
# the codeplan to NULL:
Data2df <- as.data.frame(Data2)
codeplan(Data2) <- NULL
str(Data2)
str(Data2df)
codeplan(Data2) <- NULL # Does not change anything

# Codeplans of survey items can also be inquired and manipulated:
vote <- Data1$vote
str(vote)
cp.vote <- codeplan(vote)
codeplan(vote) <- NULL
str(vote)
codeplan(vote) <- cp.vote
vote

fn.json <- paste0(tempfile(),".json")
write_codeplan(codeplan(Data1),filename=fn.json)
codeplan(Data2) <- read_codeplan(fn.json)
codeplan(Data2)
}
