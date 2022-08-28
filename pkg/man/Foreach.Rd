\name{foreach}
\alias{foreach}
\title{Loop over Variables in a Data Frame or Environment}
\description{
  \code{foreach} evaluates an expression given as untagged argument by substituting
  in variables. The expression may also contain assignments, which take effect in
  the caller's environment.
}
\usage{
  foreach(\dots,.sorted,.outer=FALSE)
}
\arguments{
  \item{\dots}{tagged and untagged arguments.
    The tagged arguments define the 'variables' that are looped over,
    the first untagged argument defines the expression wich is
    evaluated.
  }
  \item{.sorted}{an optional logical value; relevant only
    when a range of variable is specified using the column operator
    "\code{:}". Decises whether variable names should be sorted
    alphabetically before the range of variables are created.

    If this argument missing, its default value is TRUE, if \code{foreach()} is called
    in the global environment, otherwise it is FALSE.
  }
  \item{.outer}{an optional logical value; if TRUE, each combination of
    the variables is used to evaluate the expression,
    if FALSE (the default) then the variables all need to have
    the same length and the corresponding values of the
    variables are used in the evaluation of the expression.
  }
}
\examples{
x <- 1:3
y <- -(1:3)
z <- c("Uri","Schwyz","Unterwalden")
print(x)
print(y)
print(z)
foreach(var=c(x,y,z),          # assigns names
  names(var) <- letters[1:3]   # to the elements of x, y, and z
  )
print(x)
print(y)
print(z)

ds <- data.set(
        a = c(1,2,3,2,3,8,9),
        b = c(2,8,3,2,1,8,9),
        c = c(1,3,2,1,2,8,8)
      )
print(ds)
ds <- within(ds,{ 
      description(a) <- "First item in questionnaire"
      description(b) <- "Second item in questionnaire"
      description(c) <- "Third item in questionnaire"
      
      wording(a) <- "What number do you like first?"
      wording(b) <- "What number do you like second?"
      wording(c) <- "What number do you like third?"

      foreach(x=a:c,{ # Lazy data documentation:
        labels(x) <- c(    # a,b,c get value labels in one statement
                         one = 1,
                         two = 2,
                       three = 3,
                "don't know" = 8,
         "refused to answer" = 9)
        missing.values(x) <- c(8,9)
        })
      })
      
codebook(ds)

# The colon-operator respects the order of the variables
# in the data set, if .sorted=FALSE
with(ds[c(3,1,2)],
     foreach(x=a:c,
             print(description(x))
            ))

# Since .sorted=TRUE, the colon operator creates a range 
# of alphabetically sorted variables.
with(ds[c(3,1,2)],
     foreach(x=a:c,
             print(description(x)),
             .sorted=TRUE
            ))

# The variables in reverse order
with(ds,
     foreach(x=c:a,
             print(description(x))
            ))

# The colon operator can be combined with the 
# concatenation function
with(ds,
     foreach(x=c(a:b,c,c,b:a),
             print(description(x))
            ))

# Variables can also be selected by regular expressions.
with(ds,
     foreach(x=rx("[a-b]"),
             print(description(x))
            ))

# A demonstration for '.outer=TRUE'
foreach(l=letters[1:2],
        i=1:3,
        cat(paste0(l,i,"\n")),
        .outer=TRUE)

}
\keyword{programming}
