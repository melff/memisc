\name{annotations}
\alias{annotation}
\alias{annotation,ANY-method}
\alias{annotation,item-method}
\alias{annotation,data.set-method}
\alias{annotation<-}
\alias{annotation<-,ANY,character-method}
\alias{annotation<-,item,annotation-method}
\alias{annotation<-,vector,annotation-method}
\alias{description}
\alias{description,data.set-method}
\alias{description,importer-method}
\alias{description<-}
\alias{wording}
\alias{wording<-}
\alias{annotation-class}
\alias{show,annotation-method}
\title{Adding Annotations to Objects}
\description{
  Annotations, that is, objects of class \code{"annotation"},
  are character vectors with all their elements named.
  Only one method is defined for this subclass of character vectors,
  a method for \code{\link{show}}, that shows the annotation in
  a nicely formatted way. Annotations of an object can be obtained
  via the function \code{annotation(x)} and can be set via
  \code{annotation(x)<-value}.

  Elements of an annotation with names \code{"description"}
  and \code{"wording"} have a special meaning.
  The first kind can be obtained and set via
  \code{description(x)} and \code{description(x)<-value},
  the second kind can be obtained via
  \code{wording(x)} and \code{wording(x)<-value}.
  \code{"description"} elements are used in way the "variable labels"
  are used in SPSS and Stata. \code{"wording"} elements of annotation
  objects are meant to contain the question wording of a questionnaire
  item represented by an \code{"item"} objects.
  These elements of annotations are treated in a special way
  in the output of the \code{coodbook} function.
}
\usage{
annotation(x)
\S4method{annotation}{ANY}(x)
\S4method{annotation}{item}(x)
\S4method{annotation}{data.set}(x)
annotation(x)<-value
\S4method{annotation}{ANY,character}(x)<-value
\S4method{annotation}{item,annotation}(x)<-value
\S4method{annotation}{vector,annotation}(x)<-value

description(x)
description(x)<-value

wording(x)
wording(x)<-value

\S4method{description}{data.set}(x)
\S4method{description}{importer}(x)
}
\arguments{
  \item{x}{an object}
  \item{value}{a character or annotation object}
}
\value{
\code{annotation(x)} returns an object of class \code{"annotation"},
which is a named character.
\code{description(x)} and \code{wording(x)} each usually return a character string.
If \code{description(x)} is applied to a \code{\link{data.set}} or an \code{\link{importer}} object,
however, a character vector is returned, which is named after the
variables in the data set or the external file.
}
\examples{
vote <- sample(c(1,2,3,8,9,97,99),size=30,replace=TRUE)
labels(vote) <- c(Conservatives         =  1,
                    Labour                =  2,
                    "Liberal Democrats"   =  3,
                    "Don't know"          =  8,
                    "Answer refused"      =  9,
                    "Not applicable"      = 97,
                    "Not asked in survey" = 99
                    )
missing.values(vote) <- c(97,99)
description(vote) <- "Vote intention"
wording(vote) <- "If a general election would take place next tuesday,
                    the candidate of which party would you vote for?"
annotation(vote)
annotation(vote)["Remark"] <- "This is not a real questionnaire item, of course ..."
codebook(vote)
}