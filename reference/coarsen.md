# Coarsen a vector into a factor with a lower number of levels

`coarsen` can be used to obtain a factor from a vector, similar to
[`cut`](https://rdrr.io/r/base/cut.html), but with less technical and
more "aesthetic" labels of the factor levels.

## Usage

``` r
coarsen(x,...)
# S3 method for class 'numeric'
coarsen(x,
        n=5,
        pretty=TRUE,
        quantiles=!pretty,
        breaks=NULL,
        brackets=FALSE,
        sep=if(brackets)";"else if(quantiles) "-" else " - ",
        left="[",
        right="]",
        range=FALSE,
        labels=NULL,
        ...)
```

## Arguments

- x:

  a vector, usually a numeric vector

- n:

  number of categories of the resulting factor

- pretty:

  a logical value, whether
  [`pretty`](https://rdrr.io/r/base/pretty.html) should be used to
  compute the breaks.

- quantiles:

  a logical value, whether
  [`quantile`](https://rdrr.io/r/stats/quantile.html) should be used to
  compute the breaks.

- breaks:

  a vector of break points or `NULL`.

- brackets:

  a logical value, whether the labels should include brackets.

- sep:

  a character string, used as a separator between upper and lower
  boundaries in the labels.

- left:

  a character string, to be used as the left bracket

- right:

  a character string, to be used as the right bracket

- range:

  a logical value, whether the minimum and maximum of `x` should be
  included into `breaks`.

- labels:

  an optional character vector of labels.

- ...:

  further arguments, passed on to
  [`pretty`](https://rdrr.io/r/base/pretty.html) or
  [`quantile`](https://rdrr.io/r/stats/quantile.html) if applicable.

## Examples

``` r
x <- rnorm(200)

table(coarsen(x))
#> 
#> -3 - -2 -2 - -1  -1 - 0   0 - 1   1 - 2   2 - 3 
#>       5      21      82      62      27       3 

table(coarsen(x,quantiles=TRUE))
#> 
#>   0%-20%  20%-40%  40%-60%  60%-80% 80%-100% 
#>       40       40       40       40       40 

table(coarsen(x,brackets=TRUE))
#> 
#> [-3;-2] [-2;-1]  [-1;0]   [0;1]   [1;2]   [2;3] 
#>       5      21      82      62      27       3 

table(coarsen(x,breaks=c(-1,0,1)))
#> 
#> -1 - 0  0 - 1 
#>     82     62 

table(coarsen(x,breaks=c(-1,0,1),
              range=TRUE,labels=letters[1:4]))
#> 
#>  a  b  c  d 
#> 26 82 62 30 
```
