# Change labels of factors or labelled objects

Function `relabel` changes the labels of a factor or any object that has
a `names`, `labels`, `value.labels`, or `variable.labels` attribute.
Function `relabel4` is an (internal) generic which is called by
`relabel` to handle S4 objects.

## Usage

``` r
# Default S3 method
relabel(x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE)
# S3 method for class 'factor'
relabel(x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE)


# S4 method for class 'item'
relabel4(x, ...)
# This is an internal method, see details.
# Use relabel(x, \dots) for 'item' objects
```

## Arguments

- x:

  An object with a `names`, `labels`, `value.labels`, or
  `variable.labels` attribute

- ...:

  A sequence of named arguments, all of type character

- gsub:

  a logical value; if TRUE, [`gsub`](https://rdrr.io/r/base/grep.html)
  is used to change the labels of the object. That is, instead of
  substituting whole labels, substrings of the labels of the object can
  changed.

- fixed:

  a logical value, passed to [`gsub`](https://rdrr.io/r/base/grep.html).
  If TRUE, substitutions are by fixed strings and not by regular
  expressions.

- warn:

  a logical value; if TRUE, a warning is issues if a a change of labels
  was unsuccessful.

## Details

This function changes the names or labels of `x` according to the
remaining arguments. If `gsub` is FALSE, argument tags are the *old*
labels, the values are the new labels. If `gsub` is TRUE, arguments are
substrings of the labels that are substituted by the argument values.

Function `relabel` is S3 generic. If its first argument is an S4 object,
it calls the (internal) `relabel4` generic function.

## Value

The object `x` with new labels defined by the ... arguments.

## Examples

``` r
  f <- as.factor(rep(letters[1:4],5))
  levels(f)
#> [1] "a" "b" "c" "d"
  F <- relabel(f,
    a="A",
    b="B",
    c="C",
    d="D"
    )
  levels(F)
#> [1] "A" "B" "C" "D"
  
  f <- as.item(f)
  labels(f)
#> 
#>  Values and labels:
#> 
#>    1 'a'
#>    2 'b'
#>    3 'c'
#>    4 'd'
#> 
  F <- relabel(f,
    a="A",
    b="B",
    c="C",
    d="D"
    )
  labels(F)
#> 
#>  Values and labels:
#> 
#>    1 'A'
#>    2 'B'
#>    3 'C'
#>    4 'D'
#> 

  # Since version 0.99.22 - the following also works:

  f <- as.factor(rep(letters[1:4],5))
  levels(f)
#> [1] "a" "b" "c" "d"
  F <- relabel(f,
    a=A,
    b=B,
    c=C,
    d=D
    )
  levels(F)
#> [1] "A" "B" "C" "D"
  
  f <- as.item(f)
  labels(f)
#> 
#>  Values and labels:
#> 
#>    1 'a'
#>    2 'b'
#>    3 'c'
#>    4 'd'
#> 
  F <- relabel(f,
    a=A,
    b=B,
    c=C,
    d=D
    )
  labels(F)
#> 
#>  Values and labels:
#> 
#>    1 'A'
#>    2 'B'
#>    3 'C'
#>    4 'D'
#> 
  
```
