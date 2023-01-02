\name{assign_if}
\alias{assign_if}
\alias{\%if\%}
\title{Assign a values to a variable for instances where a condition is
  met}
\description{
  The \code{\%if\%} operator allows to assign values to a variable only if
    a condition is met i.e. results in \code{TRUE}. It is supposed to
    be used similar to the \code{replace ... if} construct in Stata.
}
\usage{
expr \%if\% condition
# For example
# (variable <- value) \%if\% (other_variable == 0)
}
\arguments{
  \item{expr}{An expression that assigns a value to variable}
  \item{condition}{A logical vector or a an expression that evaluates
  to a logical vector}
}
\details{
  The 'value' that is assigned to the variable in \code{expr}
  should either be a scalar, a vector with as many elements as the
  condition vector has, or as many elements as the number of elements
  in the condition vector that are equal (or evaluate to) \code{TRUE}. 
}
\examples{
(test_var <- 1) \%if\% (1:7 > 3)

test_var

(test_var <- 2) \%if\% (1:7 <= 3)
test_var

(test_var <- 100*test_var) \%if\% (1:7\%\%2==0)
test_var

# This creates a warning about non-matching lengths.
(test_var <- 500:501) \%if\% (1:7 <= 3)
test_var

(test_var <- 501:503) \%if\% (1:7 <= 3)
test_var

(test_var <- 401:407) \%if\% (1:7 <= 3)
test_var
}
