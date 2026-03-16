# Vectors of Univariate Sample Statistics

`Descriptives(x)` gives a vector of sample statistics for use in
[`codebook`](https://melff.github.io/memisc/reference/codebook.md).

## Usage

``` r
Descriptives(x,...)
# S4 method for class 'atomic'
Descriptives(x, weights = NULL, ...)
# S4 method for class 'item.vector'
Descriptives(x, weights = NULL, ...)
```

## Arguments

- x:

  an atomic vector or `"item.vector"` object.

- weights:

  an optional vector of weights.

- ...:

  further arguments, to be passed to future methods.

## Value

A numeric vector of sample statistics, containing the range, the mean,
the standard deviation, the skewness and the (excess) kurtosis.

## Examples

``` r
x <- rnorm(100)
Descriptives(x)
#>        Min        Max       Mean   Std.Dev.   Skewness   Kurtosis 
#> -2.6123343  2.7554176  0.0707892  1.0456374 -0.2090662  0.3081869 
```
