# Change dimnames, rownames, or colnames

These functions provide an easy way to change the `dimnames`, `rownames`
or `colnames` of an array.

## Usage

``` r
dimrename(x, dim = 1, ..., gsub = FALSE, fixed = TRUE, warn = TRUE)
rowrename(x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE)
colrename(x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE)
```

## Arguments

- x:

  An array with dimnames

- dim:

  A vector that indicates the dimensions

- ...:

  A sequence of named arguments

- gsub:

  a logical value; if TRUE, [`gsub`](https://rdrr.io/r/base/grep.html)
  is used to change the `dimnames` of the object. That is, instead of
  substituting whole names, substrings of the `dimnames` of the object
  can changed.

- fixed:

  a logical value, passed to [`gsub`](https://rdrr.io/r/base/grep.html).
  If TRUE, substitutions are by fixed strings and not by regular
  expressions.

- warn:

  logical; should a warning be issued if the pattern is not found?

## Details

`dimrename` changes the dimnames of `x` along dimension(s) `dim`
according to the remaining arguments. The argument names are the *old*
names, the values are the new names. `rowrename` is a shorthand for
changing the rownames, `colrename` is a shorthand for changing the
colnames of a matrix or matrix-like object.

If `gsub` is FALSE, argument tags are the *old* `dimnames`, the values
are the new `dimnames`. If `gsub` is TRUE, arguments are substrings of
the `dimnames` that are substituted by the argument values.

## Value

Object `x` with changed dimnames.

## Examples

``` r
m <- matrix(1,2,2)
rownames(m) <- letters[1:2]
colnames(m) <- LETTERS[1:2]
m
#>   A B
#> a 1 1
#> b 1 1
dimrename(m,1,a="first",b="second")
#>        A B
#> first  1 1
#> second 1 1
dimrename(m,1,A="first",B="second")
#> Warning: unused dimname(s) selected
#> Warning: unused dimname(s) selected
#>   A B
#> a 1 1
#> b 1 1
dimrename(m,2,"A"="first",B="second")
#>   first second
#> a     1      1
#> b     1      1

rowrename(m,a="first",b="second")
#>        A B
#> first  1 1
#> second 1 1
colrename(m,"A"="first",B="second")
#>   first second
#> a     1      1
#> b     1      1

# Since version 0.99.22 - the following also works:

dimrename(m,1,a=first,b=second)
#>        A B
#> first  1 1
#> second 1 1
dimrename(m,1,A=first,B=second)
#> Warning: unused dimname(s) selected
#> Warning: unused dimname(s) selected
#>   A B
#> a 1 1
#> b 1 1
dimrename(m,2,A=first,B=second)
#>   first second
#> a     1      1
#> b     1      1
```
