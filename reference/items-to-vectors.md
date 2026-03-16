# How Survey Items Are Converted into "Ordinary" Data Vectors

Survey item objects in are numeric or character vectors with some extra
information that may helpful for for managing and documenting survey
data, but they are not suitable for statistical data analysis. To run
regressions etc. one should convert
[`item`](https://melff.github.io/memisc/reference/items.md) objects into
"ordinary" numeric vectors or factors. This means that codes or values
declared as "missing" (if present) are translated into the generial
missing value `NA`, while value labels (if defined) are translated into
factor levels.

## Usage

``` r
# The following methods can be used to covert items into
# vectors with a given mode or into factors. 
# S4 method for class 'item'
as.vector(x, mode = "any")
# S4 method for class 'item'
as.numeric(x, ...)
# S4 method for class 'item'
as.integer(x, ...)
# S4 method for class 'item.vector'
as.factor(x)
# S4 method for class 'item.vector'
as.ordered(x)
# S4 method for class 'item.vector'
as.character(x, use.labels = TRUE, include.missings = FALSE, ...)
# S4 method for class 'datetime.item.vector'
as.character()
# S4 method for class 'Date.item.vector'
as.character()
# The following methods are unlikely to be useful in practice, other than
# that they are called internally by the 'as.data.frame()' method for "data.set"
# objects.
# S3 method for class 'character.item'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
# S3 method for class 'double.item'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
# S3 method for class 'integer.item'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
# S3 method for class 'Date.item'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
# S3 method for class 'datetime.item'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  an object in class "item","item.vector", etc., as relevant for the
  respective conversion method.

- mode:

  the mode of the vector to be returned, usually `"numeric"`,
  `"integer"`, or `"charcater"`

- use.labels:

  logical,should value labels be used for creating the character vector?

- include.missings:

  logical; if `TRUE`, declared missing values are not converted into
  `NA`, but into character strings with `"*"` as the "missingness
  marker" added at the beginning.

- row.names:

  optional row names, see
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)

- optional:

  a logical value, see
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)

- ...:

  other arguments, ignored.

## Value

The function [`as.vector()`](https://rdrr.io/r/base/vector.html) returns
a logical, numeric, or character depending on the `mode=` argument. If
`mode="any"`, the vector has the mode that corresponds to the (internal)
mode of the item vector, that is, an item in class "integer.item" will
become an integer vector, an item in class "double.item" will become a
double-precision numeric vector, an item in class "character.item" will
become a character vector; since the internal mode of a "dateitem.item"
or a "Date.item" vector is numeric, a numeric vector will be returned.

The functions [`as.integer()`](https://rdrr.io/r/base/integer.html),
[`as.numeric()`](https://rdrr.io/r/base/numeric.html),
[`as.character()`](https://rdrr.io/r/base/character.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html), and
[`as.ordered()`](https://rdrr.io/r/base/factor.html) return an integer,
numeric, or character vector, or an ordered or unordered factor,
respectively.

When [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) is
applied to an survey item object, the result is a single-column data
frame, where the single column is a numeric vector or character vector
or factor depending on the
[`measurement`](https://melff.github.io/memisc/reference/measurement.md)
attribute of the item. In particular, if the
[`measurement`](https://melff.github.io/memisc/reference/measurement.md)
attribute equals `"ratio"` or `"interval"` this column will be the
result of [`as.vector()`](https://rdrr.io/r/base/vector.html), if the
[`measurement`](https://melff.github.io/memisc/reference/measurement.md)
attribute equals `"ordinal"` this column will be an ordered factor (see
[`ordered`](https://rdrr.io/r/base/factor.html)), and if the
[`measurement`](https://melff.github.io/memisc/reference/measurement.md)
attribute equals `"nominal"` this column will be an unordered factor
(see [`factor`](https://rdrr.io/r/base/factor.html)).

All these functions have in common that values declared as "missing" by
virtue of the
[`value.filter`](https://melff.github.io/memisc/reference/value-filters.md)
attribute will be turned into `NA`.

## See also

[`items`](https://melff.github.io/memisc/reference/items.md)
[`annotation`](https://melff.github.io/memisc/reference/annotations.md)
[`labels`](https://melff.github.io/memisc/reference/labels.md)
[`value.filter`](https://melff.github.io/memisc/reference/value-filters.md)

## Examples

``` r
  x <- as.item(rep(1:5,4),
      labels=c(
          "First"      = 1,
          "Second"     = 2,
          "Third"      = 3,
          "Fourth"     = 4,
          "Don't know" = 5
        ),
      missing.values=5,
      annotation = c(
        description="test"
      ))
  str(x)
#>  Nmnl. item w/ 5 labels for 1,2,3,... + ms.v.  int [1:20] 1 2 3 4 5 1 2 3 4 5 ...
  summary(x)
#>       First      Second       Third      Fourth *Don't know 
#>           4           4           4           4           4 
  as.numeric(x)
#>  [1]  1  2  3  4 NA  1  2  3  4 NA  1  2  3  4 NA  1  2  3  4 NA
#> attr(,"label")
#> [1] "test"

  test <- as.item(rep(1:6,2),labels=structure(1:6,
                                      names=letters[1:6]))

  as.factor(test)
#>  [1] a b c d e f a b c d e f
#> Levels: a b c d e f
  as.numeric(test)
#>  [1] 1 2 3 4 5 6 1 2 3 4 5 6
  as.character(test)
#>  [1] "a" "b" "c" "d" "e" "f" "a" "b" "c" "d" "e" "f"
  as.character(test,include.missings=TRUE)
#>  [1] "a" "b" "c" "d" "e" "f" "a" "b" "c" "d" "e" "f"

  as.data.frame(test)[[1]]
#>  [1] a b c d e f a b c d e f
#> Levels: a b c d e f
```
