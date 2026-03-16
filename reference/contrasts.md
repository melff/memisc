# Convenience Methods for Setting Contrasts

This package provides modified versions of `contr.treatment` and
`contr.sum`. `contr.sum` gains an optional `base` argument, analog to
the one of `contr.treatment`, furthermore, the `base` argument may be
the name of a factor level.

`contr` returns a function that calls either `contr.treatment`,
`contr.sum`, etc., according to the value given to its first argument.

The `contrasts` method for `"item"` objects returns a contrast matrix or
a function to produce a contrast matrix for the factor into which the
item would be coerced via `as.factor` or `as.ordered`. This matrix or
function can be specified by using `contrasts(x)<-value`

## Usage

``` r
contr(type,...)
contr.treatment(n, base=1,contrasts=TRUE)
contr.sum(n,base=NULL,contrasts=TRUE)
# S4 method for class 'item'
contrasts(x,contrasts=TRUE,...)
# S4 method for class 'item'
contrasts(x, how.many) <- value
# These methods are defined implicitely by making 'contrasts' generic.
# S4 method for class 'ANY'
contrasts(x,contrasts=TRUE,...)
# S4 method for class 'ANY'
contrasts(x, how.many) <- value
```

## Arguments

- type:

  a character vector, specifying the type of the contrasts. This
  argument should have a value such that, if e.g. `type="something"`,
  then there is a function `contr.something` that produces a contrast
  matrix.

- ...:

  further arguments, passed to `contr.treatment`, etc.

- n:

  a number of factor levels or a vector of factor levels names, see e.g.
  `contr.treatment`.

- base:

  a number of a factor level or the names of a factor level, which
  specifies the baseline category, see e.g. `contr.treatment` or NULL.

- contrasts:

  a logical value, see
  [`contrasts`](https://rdrr.io/r/stats/contrasts.html)

- how.many:

  the number of contrasts to generate, see
  [`contrasts`](https://rdrr.io/r/stats/contrasts.html)

- x:

  a factor or an object of class "item"

- value:

  a matrix, a function or the name of a function

## Value

`contr` returns a funtion that calls one of `contr.treatment`,
`contr.sum,...`. `contr.treatment` and `contr.sum` return contrast
matrices. `contrasts(x)` returns the "contrasts" attribute of an object,
which may be a function name, a function, a contrast matrix or NULL.

## Examples

``` r
ctr.t <- contr("treatment",base="c")
ctr.t
#> function (n, contrasts = TRUE) 
#> contr.treatment(n = n, base = "c", contrasts = contrasts)
#> <environment: 0x564c0973dd90>
ctr.s <- contr("sum",base="c")
ctr.h <- contr("helmert")
ctr.t(letters[1:7])
#>   a b d e f g
#> a 1 0 0 0 0 0
#> b 0 1 0 0 0 0
#> c 0 0 0 0 0 0
#> d 0 0 1 0 0 0
#> e 0 0 0 1 0 0
#> f 0 0 0 0 1 0
#> g 0 0 0 0 0 1
ctr.s(letters[1:7])
#>    a  b  d  e  f  g
#> a  1  0  0  0  0  0
#> b  0  1  0  0  0  0
#> c -1 -1 -1 -1 -1 -1
#> d  0  0  1  0  0  0
#> e  0  0  0  1  0  0
#> f  0  0  0  0  1  0
#> g  0  0  0  0  0  1
ctr.h(letters[1:7])
#>   [,1] [,2] [,3] [,4] [,5] [,6]
#> a   -1   -1   -1   -1   -1   -1
#> b    1   -1   -1   -1   -1   -1
#> c    0    2   -1   -1   -1   -1
#> d    0    0    3   -1   -1   -1
#> e    0    0    0    4   -1   -1
#> f    0    0    0    0    5   -1
#> g    0    0    0    0    0    6

x <- factor(rep(letters[1:5],3))
contrasts(x)
#>   b c d e
#> a 0 0 0 0
#> b 1 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1
x <- as.item(x)
contrasts(x)
#>   b c d e
#> a 0 0 0 0
#> b 1 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1
contrasts(x) <- contr.sum(letters[1:5],base="c")
contrasts(x)
#>    a  b  d  e
#> a  1  0  0  0
#> b  0  1  0  0
#> c -1 -1 -1 -1
#> d  0  0  1  0
#> e  0  0  0  1
missing.values(x) <- 5
contrasts(x)
#> Warning: contrast matrix has wrong rows, deleting it
#> NULL
contrasts(as.factor(x))
#> Warning: contrast matrix has wrong rows, deleting it
#>   b c d
#> a 0 0 0
#> b 1 0 0
#> c 0 1 0
#> d 0 0 1

# Obviously setting missing values after specifying
# contrast matrix breaks the contrasts.
# Using the 'contr' function, however, prevents this:

missing.values(x) <- NULL
contrasts(x) <- contr("sum",base="c")
contrasts(x)
#>    a  b  d  e
#> a  1  0  0  0
#> b  0  1  0  0
#> c -1 -1 -1 -1
#> d  0  0  1  0
#> e  0  0  0  1
missing.values(x) <- 5
contrasts(x)
#>    a  b  d
#> a  1  0  0
#> b  0  1  0
#> c -1 -1 -1
#> d  0  0  1
contrasts(as.factor(x))
#>    a  b  d
#> a  1  0  0
#> b  0  1  0
#> c -1 -1 -1
#> d  0  0  1
```
