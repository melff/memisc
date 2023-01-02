\name{within-operators}
\alias{Within}
\alias{Within.data.frame}
\alias{\%$\%}
\alias{\%$$\%}
\title{Operators to abbreviate use of "with" and "within"}
\description{The operators \code{\%$\%} and \code{\%$$\%} provide
  abbrevitions for calls to \code{with()} and \code{within()}
  respectively.
  The function \code{Within()} is a variant of \code{with()} were
  the resulting data frame contains any newly created variables in the
  order in which they are created (and not in the reverse order).
}
\usage{
data \%$\% expr
data \%$$\% expr
Within(data,expr,\dots)
\method{Within}{data.frame}(data,expr,\dots)
}
\arguments{
  \item{data}{a data frame or similar object, see \code{\link{with}} and \code{\link{within}}}
  \item{expr}{a single or compound expression (i.e. several expressions
  enclosed in curly braces), see \code{\link{with}} and
  \code{\link{within}}}
  \item{\dots}{Further arguments, currently ignored}
}
\seealso{\code{\link{with}} and \code{\link{within}} in package "base".} 
\examples{
df <- data.frame(a = 1:7,
                 b = 7:1)

df

df <- within(df,{
  ab <- a + b
  a2b2 <- a^2 + b^2
})
df

df <- data.frame(a = 1:7,
                 b = 7:1)
df <- Within(df,{
  ab <- a + b
  a2b2 <- a^2 + b^2
})
df


df <- data.frame(a = 1:7,
                 b = 7:1)
df

ds <- as.data.set(df)
ds

df \%$$\% {
  ab <- a + b
  a2b2 <- a^2 + b^2
}
df

ds \%$$\% {
  ab <- a + b
  a2b2 <- a^2 + b^2
}
ds

df \%$\% c(a.ssq = sum(a^2),
         b.ssq = sum(b^2))
}
