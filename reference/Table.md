# One-Dimensional Table of Frequences and/or Percentages

`Table` is a generic function that produces a table of counts or
weighted counts and/or the corresponding percentages of an atomic
vector, factor or `"item.vector"` object. This function is intended for
use with
[`Aggregate`](https://melff.github.io/memisc/reference/genTable.md) or
[`genTable`](https://melff.github.io/memisc/reference/genTable.md). The
`"item.vector"` method is the workhorse of
[`codebook`](https://melff.github.io/memisc/reference/codebook.md).

## Usage

``` r
# S4 method for class 'atomic'
Table(x,weights=NULL,counts=TRUE,percentage=FALSE,...)
# S4 method for class 'factor'
Table(x,weights=NULL,counts=TRUE,percentage=FALSE,...)
# S4 method for class 'item.vector'
Table(x,weights=NULL,counts=TRUE,percentage=(style=="codebook"),
              style=c("table","codebook","nolabels"),
              include.missings=(style=="codebook"),
              missing.marker=if(style=="codebook") "M" else "*",...)
```

## Arguments

- x:

  an atomic vector, factor or `"item.vector"` object

- counts:

  logical value, should the table contain counts?

- percentage:

  logical value, should the table contain percentages? Either the
  `counts` or the `percentage` arguments or both should be `TRUE`.

- style:

  character string, the style of the names or rownames of the table.

- weights:

  a numeric vector of weights of the same length as `x`.

- include.missings:

  a logical value; should missing values included into the table?

- missing.marker:

  a character string, used to mark missing values in the table
  (row)names.

- ...:

  other, currently ignored arguments.

## Value

The atomic vector and factor methods return either a vector of counts or
vector of percentages or a matrix of counts and percentages. The same
applies to the `"item.vector"` vector method unless
`include.missing=TRUE` and `percentage=TRUE`, in which case total
percentages and percentages of valid values are given.

## Examples

``` r
  with(as.data.frame(UCBAdmissions),Table(Admit,Freq))
#> Admitted Rejected 
#>     1755     2771 
  Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
#>    Gender Dept Admitted Rejected
#> 1    Male    A      512      313
#> 2  Female    A       89       19
#> 3    Male    B      353      207
#> 4  Female    B       17        8
#> 5    Male    C      120      205
#> 6  Female    C      202      391
#> 7    Male    D      138      279
#> 8  Female    D      131      244
#> 9    Male    E       53      138
#> 10 Female    E       94      299
#> 11   Male    F       22      351
#> 12 Female    F       24      317

  A <- sample(c(1:5,9),size=100,replace=TRUE)
  labels(A) <- c(a=1,b=2,c=3,d=4,e=5,dk=9)
  missing.values(A) <- 9
  Table(A,percentage=TRUE)
#>     Counts  Percent
#> a 16.00000 19.51220
#> b 17.00000 20.73171
#> c 16.00000 19.51220
#> d 13.00000 15.85366
#> e 20.00000 24.39024
```
