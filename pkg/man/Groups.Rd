\name{Groups}
\alias{Groups}
\alias{Groups,data.set,formula-method}
\alias{Groups,data.frame,formula-method}
\alias{grouped.data}
\alias{with.grouped.data}
\alias{within.grouped.data}
\alias{names.grouped.data}
\alias{recombine}
\alias{recombine.grouped.data}
\alias{withGroups}
\alias{withinGroups}
\title{Operate on grouped data in data frames and data sets}
\description{\code{Group} creates a grouped variant of an object of
  class "data.frame" or of class "data.set", for which methods for
  \code{with} and \code{within} are defined, so that these well-known
  functions can be applied "groupwise".}
\usage{
# Create an object of class "grouped.data" from a
# data frame or a data set.
Groups(data,by,\dots)
\S4method{Groups}{data.frame,formula}(data,by,\dots)
\S4method{Groups}{data.set,formula}(data,by,\dots)

# Recombine grouped data into a data fame or a data set
recombine(x,\dots)
\method{recombine}{grouped.data}(x,\dots)

# Methods of the generics "with" and "within" for grouped data
\method{with}{grouped.data}(data,expr,\dots)
\method{within}{grouped.data}(data,expr,recombine=FALSE,\dots)
\method{names}{grouped.data}(x)

# This is equivalent to with(Groups(data,by),expr,...)
withGroups(data,by,expr,\dots)
# This is equivalent to within(Groups(data,by),expr,recombine,...)
withinGroups(data,by,expr,recombine=TRUE,\dots)
}
\arguments{
  \item{data}{an object of the classes "data.frame", "data.set" if an
    argument to \code{Groups}, \code{withGroups}, \code{withinGroups},
  }
  \item{by}{a formula with the factors the levels of which define the
    groups.}
  \item{expr}{an expression, or several expressions enclosed in curly
    braces.}
  \item{recombine}{a logical vector; should the resulting grouped
    data be recombined?}
  \item{x}{an object of class "grouped.data".}
  \item{\dots}{other arguments, ignored.}
}
\examples{
some.data <- data.frame(x=rnorm(n=100))
some.data <- within(some.data,{
    f <- factor(rep(1:4,each=25),labels=letters[1:4])
    g <- factor(rep(1:5,each=4,5),labels=LETTERS[1:5])
    y <- x + rep(1:4,each=25) +  0.75*rep(1:5,each=4,5)
})


some.grouped.data <- Groups(some.data,
                           ~f+g)    

group.means <- with(some.grouped.data,
                    mean(y))
group.means

some.grouped.data <- within(some.grouped.data,{
    y.cent <- y - mean(y)
},recombine=FALSE)

group.means <- with(some.grouped.data,
                    round(mean(y.cent),15))
group.means

str(group.means)

with(some.grouped.data,
     c(Centered=round(mean(y.cent),15),
       Uncentered=mean(y)))

some.data.ungrouped <- recombine(some.grouped.data)
str(some.data.ungrouped)

some.dataset <- as.data.set(some.data)
some.grouped.dataset <- Groups(some.dataset,~f+g)

with(some.grouped.dataset,
     c(Mean=mean(y),
       Variance=var(y)))


with(Groups(some.data,~f+g),mean(y))

some.data <- within(Groups(some.data,~f+g),{
    y.cent <- y - mean(y)
},recombine=TRUE)

round(with(some.data,
           tapply(y.cent,list(g,f),
                  mean,na.rm=TRUE)),15)

some.data <- withinGroups(some.data,~f+g,{
    y.cent <- y - mean(y)
})

round(with(some.data,
           tapply(y.cent,list(g,f),
                  mean)),15)


withGroups(some.data,~f+g,{
    round(mean(y.cent),15)
})
}
