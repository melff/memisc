# Value Filters

Value filters, that is objects that inherit from class "value.filter",
are a mechanism to distinguish between valid codes of a survey item and
codes that are considered to be missing, such as the codes for answers
like "don't know" or "answer refused".

Value filters are optional slot values of "item" objects. They determine
which codes of "item" objects are replaced by `NA` when they are coerced
into a vector or a factor.

There are three (sub)classes of value filters: "missing.values", which
specify individual missing values and/or a range of missing values;
"valid.values", which specify individual valid values (that is, all
other values of the item are considered as missing); "valid.range",
which specify a range of valid values (that is, all values outside the
range are considered as missing). Value filters of class
"missing.values" correspond to missing-values declarations in SPSS
files, imported by
[`spss.fixed.file`](https://melff.github.io/memisc/reference/importers.md),
[`spss.portable.file`](https://melff.github.io/memisc/reference/importers.md),
or
[`spss.system.file`](https://melff.github.io/memisc/reference/importers.md).

Value filters also can be updated using the `+` and `-` operators.

## Usage

``` r
value.filter(x)

missing.values(x)
missing.values(x) <- value

valid.values(x)
valid.values(x) <- value

valid.range(x)
valid.range(x) <- value

is.valid(x)
nvalid(x)
is.missing(x)
include.missings(x,mark="*")
```

## Arguments

- x,value:

  objects of the appropriate class.

- mark:

  a character string, used to pasted to value labels of `x` (if
  present).

## Value

`value.filter(x)`, `missing.values(x)`, `valid.values(x)`, and
`valid.range(x)`, return the value filter associated with `x`, an object
of class "value.filter", that is, of class "missing.values",
"valid.values", or "valid.range", respectively.

`is.missing(x)` returns a logical vector indicating for each element of
`x` whether it is a missing value or not. `is.valid(x)` returns a
logical vector indicating for each element of `x` whether it is a valid
value or not. `nvalid(x)` returns the number of elements of `x` that are
valid.

For convenience, `is.missing(x)` and `is.valid(x)` also work for atomic
vectors and factors, where they are equivalent to `is.na(x)` and
`!is.na(x)`. For atomic vectors and factors, `nvalid(x)` returns the
number of elements of `x` for which `!is.na(x)` is TRUE.

`include.missings(x,...)` returns a copy of `x` that has all values
declared as valid.

## Examples

``` r
x <- rep(c(1:4,8,9),2,length=60)
labels(x) <- c(
    a=1,
    b=2,
    c=3,
    d=4,
    dk=8,
    refused=9
    )
missing.values(x) <- 9
missing.values(x)
#> 9
missing.values(x) <- missing.values(x) + 8
missing.values(x)
#> 9, 8
missing.values(x) <- NULL
missing.values(x)
#> NULL
missing.values(x) <- list(range=c(8,Inf))
missing.values(x)
#> 8 - Inf
valid.values(x)
#> 1, 2, 3, 4
print(x)
#>  [1]        a        b        c        d      *dk *refused        a        b
#>  [9]        c        d      *dk *refused        a        b        c        d
#> [17]      *dk *refused        a        b        c        d      *dk *refused
#> [25]        a        b        c        d      *dk *refused        a        b
#> [33]        c        d      *dk *refused        a        b        c        d
#> [41]      *dk *refused        a        b        c        d      *dk *refused
#> [49]        a        b        c        d      *dk *refused        a        b
#> [57]        c        d      *dk *refused
is.missing(x)
#>  [1] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [13] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [25] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [37] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [49] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
is.valid(x)
#>  [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
#> [13]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
#> [25]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
#> [37]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
#> [49]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
as.factor(x)
#>  [1] a    b    c    d    <NA> <NA> a    b    c    d    <NA> <NA> a    b    c   
#> [16] d    <NA> <NA> a    b    c    d    <NA> <NA> a    b    c    d    <NA> <NA>
#> [31] a    b    c    d    <NA> <NA> a    b    c    d    <NA> <NA> a    b    c   
#> [46] d    <NA> <NA> a    b    c    d    <NA> <NA> a    b    c    d    <NA> <NA>
#> Levels: a b c d
as.factor(include.missings(x))
#>  [1] a        b        c        d        *dk      *refused a        b       
#>  [9] c        d        *dk      *refused a        b        c        d       
#> [17] *dk      *refused a        b        c        d        *dk      *refused
#> [25] a        b        c        d        *dk      *refused a        b       
#> [33] c        d        *dk      *refused a        b        c        d       
#> [41] *dk      *refused a        b        c        d        *dk      *refused
#> [49] a        b        c        d        *dk      *refused a        b       
#> [57] c        d        *dk      *refused
#> Levels: a b c d *dk *refused
as.integer(x)
#>  [1]  1  2  3  4 NA NA  1  2  3  4 NA NA  1  2  3  4 NA NA  1  2  3  4 NA NA  1
#> [26]  2  3  4 NA NA  1  2  3  4 NA NA  1  2  3  4 NA NA  1  2  3  4 NA NA  1  2
#> [51]  3  4 NA NA  1  2  3  4 NA NA
as.integer(include.missings(x))
#>  [1] 1 2 3 4 8 9 1 2 3 4 8 9 1 2 3 4 8 9 1 2 3 4 8 9 1 2 3 4 8 9 1 2 3 4 8 9 1 2
#> [39] 3 4 8 9 1 2 3 4 8 9 1 2 3 4 8 9 1 2 3 4 8 9
```
