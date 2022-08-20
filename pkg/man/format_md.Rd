\name{format_md}
\alias{format_md}
\alias{format_md.codebook}
\alias{format_md.codebookEntry}
\title{
  Format Codebooks as Markdown
}
\description{
 \code{format_md} is for showing objects in a convenient way in Markdown
format. Can be included to Rmarkdown file with the \code{cat()} function and the
\code{results='asis'} code block option. The following example should be runned
in a Rmd file with different output formats.
}
\usage{
\method{format_md}{codebook}(x, \dots)
\method{format_md}{codebookEntry}(x, name = "", add_rules = TRUE, \dots)
}
\arguments{
  \item{x}{a "codebook" or "codebookEntry" object}
  \item{name}{a string; the variable name}
  \item{add_rules}{a boolean value; if TRUE adds a horizontal rules before and after the title}
  \item{\dots}{further arguments, passed to other functions}
  }
\value{
   \code{format_md} character string with code suitable for inclusion into a Markdown-file.
}
\examples{
library(memisc)

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

codebook_data <- codebook(Data1)

codebook_md <- format_md(codebook_data, digits = 2)

writeLines(codebook_md)

\dontrun{
writeLines(codebook_md,con="codebook-example.md")
}
 
}
