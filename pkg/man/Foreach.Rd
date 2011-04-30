\name{foreach}
\alias{foreach}
\title{Loop over Variables in a Data Frame or Environment}
\description{
  \code{foreach} evaluates an expression given as second argument by substituting
  in variables. The expression may also contain assignments, which take effect in
  the callers environment.
}
\usage{
  foreach(\dots)
}
\arguments{
  \item{\dots}{tagged and untagged arguments.
    The tagged arguments define the 'variables' that are looped over,
    the first untagged argument defines the expression wich is
    evaluated.
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

      foreach(x=c(a,b,c),{ # Lazy data documentation:
        labels(x) <- c(    # a,b,c get value labels in one statement
                         one = 1,
                         two = 2,
                       three = 3,
                "don't know" = 8,
         "refused to answer" = 9)
        missing.values(x) <- c(8,9)
        })
      })
      
as.data.frame(ds)

ds <- within(ds,foreach(x=c(a,b,c),{
  measurement(x) <- "interval"
  }))

as.data.frame(ds)
}
\keyword{programming}