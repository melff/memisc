\name{format_md}
\alias{format_md}
\alias{format_md.codebook}
\alias{format_md.codebookEntry}
\title{
  Format Codebooks as Markdown
}
\description{
 \code{format_md} is for showing objects in a convenient way in Markdown format. Can be included to Rmarkdown file with the \code{cat()} function and the \code{results='asis'} code block option. The following example should be runned in a Rmd file with different output formats.
}
\usage{
\method{format_md}{codebook}(x, unlist = TRUE, collapse = TRUE, ...)
\method{format_md}{codebookEntry}(x, name = "", add_lines = TRUE, ...)
}
\arguments{
  \item{x}{a "codebook" or "codebookEntry" object}
  \item{unlist}{a boolean; if false one list per item with the Markdown output}
  \item{collapse}{a boolean; if false one element for each line}
  \item{name}{a string; the variable name}
  \item{add_lines}{a boolean; if true adds a lines before and after the title}
  \item{\dots}{further arguments, passed to other functions}
  }
\value{
   \code{format_md} character string with code suitable for inclusion into a Markdown-file.
}
\examples{
\dontrun{
 ---
 title: "Title"
 author: "Author"
 date: "01/01/2000"
 output: html_document
 ---

 ## The table can be formated with knitr::kable options
   
 ```{r setup, include=FALSE}
 library(memisc)
 opts <- options(knitr.kable.NA = "", digits = 2)
 ```
 
 ```{r, include=FALSE}
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
 
 ```
 
 ## Codebook
 
 ```{r cars, echo=FALSE, results='asis'}
 
 codebook_data <- codebook(Data1)
 
 codebook_md <- format_md(codebook_data, digits = 2)
 
 cat(codebook_md, sep = "\n")
 
 ```

 ## Codebook without the vote variable
 
 ```{r cars, echo=FALSE, results='asis'}
 
 codebook_md2 <- format_md(codebook_data, digits = 2, unlist = FALSE)

 codebook_md2[["vote"]] <- NULL
 
 cat(codebook_md2, sep = "\n")
 
 ```
  }
}
