# Convenience Methods to Sort Data Frames and Data Sets

The methods below return a sorted version of the data frame or data set,
given as first argument.

## Usage

``` r
# S3 method for class 'data.frame'
sort(x,decreasing=FALSE,by=NULL,na.last=NA,...)
# S3 method for class 'data.set'
sort(x,decreasing=FALSE,by=NULL,na.last=NA,...)
```

## Arguments

- x:

  a data frame or data set.

- decreasing:

  a logical value, should sorting be in increasing or decreasing order?

- by:

  a character name of variable names, by which to sort; a formula giving
  the variables, by which to sort; NULL, in which case, the data frame /
  data set is sorted by all of its variables.

- na.last:

  for controlling the treatment of 'NA's. If 'TRUE', missing values in
  the data are put last; if 'FALSE', they are put first; if 'NA', they
  are removed

- ...:

  other arguments, currently ignored.

## Value

A sorted copy of `x`.

## Examples

``` r
DF <- data.frame(
        a = sample(1:2,size=20,replace=TRUE),
        b = sample(1:4,size=20,replace=TRUE))
sort(DF)
#>    a b
#> 2  1 1
#> 19 1 1
#> 3  1 2
#> 11 1 2
#> 16 1 2
#> 5  1 4
#> 8  1 4
#> 9  1 4
#> 15 1 4
#> 17 1 4
#> 18 1 4
#> 20 1 4
#> 14 2 2
#> 4  2 3
#> 6  2 3
#> 7  2 3
#> 12 2 3
#> 13 2 3
#> 1  2 4
#> 10 2 4
sort(DF,by=~a+b)
#>    a b
#> 2  1 1
#> 19 1 1
#> 3  1 2
#> 11 1 2
#> 16 1 2
#> 14 2 2
#> 4  2 3
#> 6  2 3
#> 7  2 3
#> 12 2 3
#> 13 2 3
#> 5  1 4
#> 8  1 4
#> 9  1 4
#> 15 1 4
#> 17 1 4
#> 18 1 4
#> 20 1 4
#> 1  2 4
#> 10 2 4
sort(DF,by=~b+a)
#>    a b
#> 2  1 1
#> 19 1 1
#> 3  1 2
#> 11 1 2
#> 16 1 2
#> 5  1 4
#> 8  1 4
#> 9  1 4
#> 15 1 4
#> 17 1 4
#> 18 1 4
#> 20 1 4
#> 14 2 2
#> 4  2 3
#> 6  2 3
#> 7  2 3
#> 12 2 3
#> 13 2 3
#> 1  2 4
#> 10 2 4
sort(DF,by=c("b","a"))
#>    a b
#> 2  1 1
#> 19 1 1
#> 3  1 2
#> 11 1 2
#> 16 1 2
#> 14 2 2
#> 4  2 3
#> 6  2 3
#> 7  2 3
#> 12 2 3
#> 13 2 3
#> 5  1 4
#> 8  1 4
#> 9  1 4
#> 15 1 4
#> 17 1 4
#> 18 1 4
#> 20 1 4
#> 1  2 4
#> 10 2 4
sort(DF,by=c("a","b"))
#>    a b
#> 2  1 1
#> 19 1 1
#> 3  1 2
#> 11 1 2
#> 16 1 2
#> 5  1 4
#> 8  1 4
#> 9  1 4
#> 15 1 4
#> 17 1 4
#> 18 1 4
#> 20 1 4
#> 14 2 2
#> 4  2 3
#> 6  2 3
#> 7  2 3
#> 12 2 3
#> 13 2 3
#> 1  2 4
#> 10 2 4
```
