\name{recode}
\alias{recode}
\alias{recode,vector-method}
\alias{recode,item-method}
\alias{recode,factor-method}
\title{Recode Items, Factors and Numeric Vectors}
\description{
  \code{recode} substitutes old values of a factor or a numeric
  vector by new ones, just like the recoding facilities in some
  commercial statistical packages.
}
\usage{
  \S4method{recode}{vector}(x,...,otherwise="NA")
  \S4method{recode}{factor}(x,...,otherwise="NA")
  \S4method{recode}{item}(x,...,otherwise="NA")
}
\arguments{
  \item{x}{An object}
  \item{\dots}{One or more assignment expressions, each 
    of the form \code{new.value <- old.values}.
    \code{new.value} should be a scalar numeric value
    or character string. If one of the \code{new.value}s
    is a character string, the return value
    of \code{recode} will be a factor and each \code{new.value}
    will be coerced to a character string that labels a level of the factor.

    Each \code{old.value} in an assignment expression may be a
    (numeric or character) vector. If \code{x} is numeric such an
    assignment expression may have the form \code{new.value <- range(lower,upper)}
    In that case, values between \code{lower} and \code{upper} are exchanged by
    \code{new.value}. If one of the arguments to \code{range} is \code{min},
    it is substituted by the minimum of \code{x}.
    If one of the arguments to \code{range} is \code{max},
    it is substituted by the maximum of \code{x}.

    In case of the method for \code{labelled} vectors, the \emph{tags} of
    arguments of the form \code{tag = new.value <- old.values}
    will define the labels of the new codes.

    If the \code{old.values} of different assignment expressions overlap,
    an error will be raised because the recoding is ambigous.
    }
  \item{otherwise}{a character string or some other value
    that the result may obtain. If equal to \code{NA} or \code{"NA"},
    original codes not given an explicit new code are recoded into
    \code{NA}. If equal to \code{"copy"},
    original codes not given an explicit new code are copied.
    }
}

\value{
  A numerical vector, factor or an \code{item} object.
}

\details{
\code{recode} relies on the lazy evaluation mechanism of \emph{R}:
Arguments are not evaluated until required by the function they are given to.
\code{recode} does not cause arguments that appear in \code{\dots} to be evaluated.
Instead, \code{recode} parses the \code{\dots} arguments. Therefore, although
expressions like \code{1 <- 1:4} would cause an error action, if evaluated
at any place elsewhere in \emph{R}, they will not cause an error action,
if given to \code{recode} as an argument. However, a call of the
form \code{recode(x,1=1:4)}, would be a syntax error.

If John Fox' package "car" is installed, \code{recode} will also be callable
with the syntax of the \code{\link[car]{recode}} function of that package.
}

\seealso{
  \code{\link[car]{recode}} of package \code{car}.
}

\examples{
x <- as.item(sample(1:6,20,replace=TRUE),
        labels=c( a=1,
                  b=2,
                  c=3,
                  d=4,
                  e=5,
                  f=6))
print(x)

# A recoded version of x is returned
# containing the values 1, 2, 3, which are
# labelled as "A", "B", "C".
recode(x,
  A = 1 <- range(min,2),
  B = 2 <- 3:4,
  C = 3 <- range(5,max), # this last comma is ignored
  )

# This causes an error action: the sets
# of original values overlap.
try(recode(x,
  A = 1 <- range(min,2),
  B = 2 <- 2:4,
  C = 3 <- range(5,max)
  ))

recode(x,
  A = 1 <- range(min,2),
  B = 2 <- 3:4,
  C = 3 <- range(5,6),
  D = 4 <- 7
  )
  
# This results in an all-missing vector:
recode(x,
  D = 4 <- 7,
  E = 5 <- 8
  )

f <- as.factor(x)
x <- as.integer(x)

recode(x,
  1 <- range(min,2),
  2 <- 3:4,
  3 <- range(5,max)
  )

# This causes another error action:
# the third argument is an invalid
# expression for a recoding.
try(recode(x,
  1 <- range(min,2),
  3:4,
  3 <- range(5,max)
  ))

# The new values are character strings,
# therefore a factor is returned.
recode(x,
  "a" <- range(min,2),
  "b" <- 3:4,
  "c" <- range(5,6)
  )
  
recode(x,
  1 <- 1:3,
  2 <- 4:6
  )
  
recode(x,
  4 <- 7,
  5 <- 8,
  otherwise = "copy"
  )

recode(f,
  "A" <- c("a","b"),
  "B" <- c("c","d"),
  otherwise="copy"
  )

recode(f,
  "A" <- c("a","b"),
  "B" <- c("c","d"),
  otherwise="C"
  )
 
recode(f,
  "A" <- c("a","b"),
  "B" <- c("c","d")
  )

}
\keyword{manip}
