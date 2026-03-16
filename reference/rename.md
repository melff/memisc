# Change Names of a Named Object

`rename` changes the names of a named object.

## Usage

``` r
rename(x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE)
```

## Arguments

- x:

  Any named object

- ...:

  A sequence of named arguments, all of type character

- gsub:

  a logical value; if TRUE, [`gsub`](https://rdrr.io/r/base/grep.html)
  is used to change the row and column labels of the resulting table.
  That is, instead of substituting whole names, substrings of the names
  of the object can changed.

- fixed:

  a logical value, passed to [`gsub`](https://rdrr.io/r/base/grep.html).
  If TRUE, substitutions are by fixed strings and not by regular
  expressions.

- warn:

  a logical value; should a warning be issued if those names to change
  are not found?

## Details

This function changes the names of `x` according to the remaining
arguments. If `gsub` is FALSE, argument tags are the *old* names, the
values are the new names. If `gsub` is TRUE, arguments are substrings of
the names that are substituted by the argument values.

## Value

The object `x` with new names defined by the ... arguments.

## Examples

``` r
  x <- c(a=1, b=2)
  rename(x,a="A",b="B")
#> A B 
#> 1 2 
  # Since version 0.99.22 - the following also works:
  rename(x,a=A,b=B)
#> A B 
#> 1 2 
  
  str(rename(iris,
                  Sepal.Length="Sepal_Length",
                  Sepal.Width ="Sepal_Width",
                  Petal.Length="Petal_Length",
                  Petal.Width ="Petal_Width"
                  ))
#> 'data.frame':    150 obs. of  5 variables:
#>  $ Sepal_Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal_Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>  $ Petal_Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>  $ Petal_Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
  str(rename(iris,
                  .="_"
                  ,gsub=TRUE))
#> 'data.frame':    150 obs. of  5 variables:
#>  $ Sepal_Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal_Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>  $ Petal_Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>  $ Petal_Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

  # Since version 0.99.22 - the following also works:
  str(rename(iris,
                  Sepal.Length=Sepal_Length,
                  Sepal.Width =Sepal_Width,
                  Petal.Length=Petal_Length,
                  Petal.Width =Petal_Width
                  ))
#> 'data.frame':    150 obs. of  5 variables:
#>  $ Sepal_Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal_Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>  $ Petal_Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>  $ Petal_Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```
