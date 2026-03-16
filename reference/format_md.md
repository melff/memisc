# Format Codebooks as Markdown

`format_md` is for showing objects in a convenient way in Markdown
format. Can be included to Rmarkdown file with the
[`cat()`](https://rdrr.io/r/base/cat.html) function and the
`results='asis'` code block option. The following example should be
runned in a Rmd file with different output formats.

## Usage

``` r
# S3 method for class 'codebook'
format_md(x, ...)
# S3 method for class 'codebookEntry'
format_md(x, name = "", add_rules = TRUE, ...)
```

## Arguments

- x:

  a "codebook" or "codebookEntry" object

- name:

  a string; the variable name

- add_rules:

  a boolean value; if TRUE adds a horizontal rules before and after the
  title

- ...:

  further arguments, passed to other functions

## Value

`format_md` character string with code suitable for inclusion into a
Markdown-file.

## Examples

``` r
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
#> --------------------------------------------------------------------------------
#> 
#>    vote --- 'Vote intention'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double    
#>    Measurement: nominal    
#>    Missing values: 8, 9, 97, 99    
#> 
#>    | Values |   | Labels                |     N | Valid | Total |
#>    |-------:|--:|:----------------------|------:|------:|------:|
#>    |  1     |   | 'Conservatives'       |    45 |  36.9 |  15.0 |
#>    |  2     |   | 'Labour'              |    44 |  36.1 |  14.7 |
#>    |  3     |   | 'Liberal Democrats'   |    33 |  27.0 |  11.0 |
#>    |  8     | M | 'Don't know'          |    41 |       |  13.7 |
#>    |  9     | M | 'Answer refused'      |    55 |       |  18.3 |
#>    | 97     | M | 'Not applicable'      |    45 |       |  15.0 |
#>    | 99     | M | 'Not asked in survey' |    37 |       |  12.3 |
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
#> --------------------------------------------------------------------------------
#> 
#>    region --- 'Region of residence'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double    
#>    Measurement: nominal    
#>    Missing values: 97, 99    
#> 
#>    | Values |   | Labels                |     N | Valid | Total |
#>    |-------:|--:|:----------------------|------:|------:|------:|
#>    |  1     |   | 'England'             |   133 |  52.0 |  44.3 |
#>    |  2     |   | 'Scotland'            |    84 |  32.8 |  28.0 |
#>    |  3     |   | 'Wales'               |    39 |  15.2 |  13.0 |
#>    | 99     | M | 'Not asked in survey' |    44 |       |  14.7 |
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
#> --------------------------------------------------------------------------------
#> 
#>    income --- 'Household income'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double    
#>    Measurement: ratio    
#> 
#>    Min:        243.828   
#>    Max:      14279.069   
#>    Mean:      2564.207   
#>    Std.Dev.:  1957.472   
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 

if (FALSE) { # \dontrun{
writeLines(codebook_md,con="codebook-example.md")
} # }
 
```
