\name{Groups}
\alias{Groups}
\alias{Groups.data.set}
\alias{Groups.data.frame}
\alias{grouped.data}
\alias{Groups.grouped.data}
\alias{with.grouped.data}
\alias{within.grouped.data}
\alias{recombine}
\alias{recombine.grouped.data.frame}
\alias{recombine.grouped.data.set}
\alias{as.data.frame.grouped.data}
\alias{as.data.set,grouped.data.frame-method}
\alias{as.data.set,grouped.data.set-method}
\alias{withGroups}
\alias{withinGroups}
\title{Operate on grouped data in data frames and data sets}
\description{\code{Group} creates a grouped variant of an object of
  class "data.frame" or of class "data.set", for which methods for
  \code{with} and \code{within} are defined, so that these well-known
  functions can be applied "groupwise".
}
\usage{
# Create an object of class "grouped.data" from a
# data frame or a data set.
Groups(data,by,\dots)
\method{Groups}{data.frame}(data,by,\dots)
\method{Groups}{data.set}(data,by,\dots)
\method{Groups}{grouped.data}(data,by,\dots)

# Recombine grouped data into a data fame or a data set
recombine(x,\dots)
\method{recombine}{grouped.data.frame}(x,\dots)
\method{recombine}{grouped.data.set}(x,\dots)

# Recombine grouped data and coerce the result appropriately:
\method{as.data.frame}{grouped.data}(x,\dots)
\S4method{as.data.set}{grouped.data.frame}(x,row.names=NULL,\dots)
\S4method{as.data.set}{grouped.data.set}(x,row.names=NULL,\dots)

# Methods of the generics "with" and "within" for grouped data
\method{with}{grouped.data}(data,expr,\dots)
\method{within}{grouped.data}(data,expr,recombine=FALSE,\dots)

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
  \item{row.names}{an optional character vector with row names.}
  \item{\dots}{other arguments, ignored.}
}

\details{
    When applied to a data frame \code{Groups} returns an object with class attributes
    "grouped.data.frame", "grouped.data", and "data.frame", when applied do an object with class
    "data.set", it returns an object with class attributes "grouped.data.set",
    "grouped.data", and "data.set".

    When applied to objects with class attributed
    "grouped.data", both the functions \code{with()} amd \code{within()}
    evaluate \code{expr} separately for each group defined by
    \code{Groups}. \code{with()} returns an array composed of the results
    of \code{expr}, while \code{within()} returns a modified copy of its
    \code{data} argument, which will be a "grouped.data" object
    ("grouped.data.frame" or "grouped.data.set"), unless the argument
    \code{recombine=TRUE} is set.

    The expression \code{expr} may contain references to the variables
    \code{n_}, \code{N_}, and \code{i_}. \code{n_} is equal to the size of
    the respective group (the number of rows belonging to it), while
    \code{N_} is equal to the total number of observations in all
    groups. The variable \code{i_} equals to the indices of the rows
    belonging to the respective group of observations.
}

\examples{
some.data <- data.frame(x=rnorm(n=100))
some.data <- within(some.data,{
    f <- factor(rep(1:4,each=25),labels=letters[1:4])
    g <- factor(rep(1:5,each=4,5),labels=LETTERS[1:5])
    y <- x + rep(1:4,each=25) +  0.75*rep(1:5,each=4,5)
})

# For demonstration purposes, we create an
# 'empty' group:
some.data <- subset(some.data,
                       f!="a" | g!="C")

some.grouped.data <- Groups(some.data,
                           ~f+g)    

# Computing the means of y for each combination f and g
group.means <- with(some.grouped.data,
                    mean(y))
group.means

# Obtaining a groupwise centered variant of y
some.grouped.data <- within(some.grouped.data,{
    y.cent <- y - mean(y)
},recombine=FALSE)

# The groupwise centered variable should have zero mean
# whithin each group
group.means <- with(some.grouped.data,
                    round(mean(y.cent),15))
group.means

# The following demonstrates the use of n_, N_, and i_
# An external copy of y
y1 <- some.data$y
group.means.n <- with(some.grouped.data,
                      c(mean(y),  # Group means for y
                        n_,       # Group sizes
                        sum(y)/n_,# Group means for y
                        n_/N_,    # Relative group sizes
                        sum(y1)/N_,# NOT the grand mean
                        sum(y1[i_])/n_)) # Group mean for y1
group.means.n

# Names can be attached to the groupwise results
with(some.grouped.data,
     c(Centered=round(mean(y.cent),15),
       Uncentered=mean(y)))

some.data.ungrouped <- recombine(some.grouped.data)
str(some.data.ungrouped)

# It all works with "data.set" objects
some.dataset <- as.data.set(some.data)
some.grouped.dataset <- Groups(some.dataset,~f+g)

with(some.grouped.dataset,
     c(Mean=mean(y),
       Variance=var(y)))

# The following two expressions are equivalent:
with(Groups(some.data,~f+g),mean(y))
withGroups(some.data,~f+g,mean(y))

# The following two expressions are equivalent:
some.data <- within(Groups(some.data,~f+g),{
    y.cent <- y - mean(y)
    y.cent.1 <- y - sum(y)/n_
})

some.data <- withinGroups(some.data,~f+g,{
    y.cent <- y - mean(y)
    y.cent.1 <- y - sum(y)/n_
})

# Both variants of groupwise centred varaibles should
# have zero groupwise means:
withGroups(some.data,~f+g,{
    c(round(mean(y.cent),15),
      round(mean(y.cent.1),15))
})

}
