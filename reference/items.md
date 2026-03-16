# Survey Items

Objects of class `item` are data vectors with additional information
attached to them like “value labels” and “user-defined missing values”
known from software packages like SPSS or Stata.

The class `item` is intended to facilitate data management of survey
data. Objects in this class should *not* directly used in data analysis.
Instead they should changed into "ordinary" vectors or factors before.
For this see the documentation for
[`as.vector,item-method`](https://melff.github.io/memisc/reference/items-to-vectors.md).

## Usage

``` r
## The constructor for objects of class "item"
## more convenient than new("item",...)
# S4 method for class 'numeric'
as.item(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), ...
  )
# S4 method for class 'character'
as.item(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), ...
  )

# S4 method for class 'logical'
as.item(x,...)
# x is first coerced to integer,
# arguments in ... are then passed to the "numeric"
# method.

# S4 method for class 'factor'
as.item(x,...)
# S4 method for class 'ordered'
as.item(x,...)
# S4 method for class 'POSIXct'
as.item(x,...)

# S4 method for class 'double.item'
as.item(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), ...
  )

# S4 method for class 'integer.item'
as.item(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), ...
  )

# S4 method for class 'character.item'
as.item(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), ...
  )

# S4 method for class 'datetime.item'
as.item(x,
  labels=NULL,  missing.values=NULL,
  valid.values=NULL,  valid.range=NULL,
  value.filter=NULL,  measurement=NULL,
  annotation=attr(x,"annotation"), ...
  )
```

## Arguments

- x:

  for `as.item` methods, any atomic vector; for the `unique`, `summary`,
  `str`, `print`, `[`, and `<-` methods, a vector with class `labelled`.

- labels:

  a named vector of the same mode as `x`.

- missing.values:

  either a vector of the same mode as `x`, or a list with components
  `"values"`, vector of the same mode as `x` (which defines individual
  missing values) and `"range"` a matrix with two rows with the same
  mode as `x` (which defines a range of missing values), or an object of
  class `"missing.values"`.

- valid.values:

  either a vector of the same mode as `x`, defining those values of `x`
  that are to be considered as valid, or an object of class
  `"valid.values"`.

- valid.range:

  either a vector of the same mode as `x` and length 2, defining a range
  of valid values of `x`, or an object of class `"valid.range"`.

- value.filter:

  an object of class `"value.filter"`, that is, of classes
  `"missing.values"`, `"valid.values"`, or `"valid.range"`.

- measurement:

  level of measurement; one of "nominal", "ordinal", "interval", or
  "ratio".

- annotation:

  a named character vector, or an object of class `"annotation"`

- ...:

  further arguments, ignored.

## See also

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
  test
#> 
#> Item (measurement: nominal, type: integer, length = 12) 
#> 
#>  [1:12] a b c d e f a b c d e f
  test == 1
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
  test != 1
#>  [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
  test == "a"
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
  test != "a"
#>  [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
  test == c("a","z")
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
  test != c("a","z")
#>  [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
  test 
#> 
#> Item (measurement: nominal, type: integer, length = 12) 
#> 
#>  [1:12] a b c d e f a b c d e f
  test 
#> 
#> Item (measurement: nominal, type: integer, length = 12) 
#> 
#>  [1:12] a b c d e f a b c d e f

  codebook(test)
#> ================================================================================
#> 
#>    test
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   2    16.7
#>    2 'b'                   2    16.7
#>    3 'c'                   2    16.7
#>    4 'd'                   2    16.7
#>    5 'e'                   2    16.7
#>    6 'f'                   2    16.7
#> 

  Test <- as.item(rep(letters[1:6],2),
                    labels=structure(letters[1:6],
                                     names=LETTERS[1:6]))
  Test
#> 
#> Item (measurement: nominal, type: character, length = 12) 
#> 
#>  [1:12] A B C D E F A B C D E F
  Test == "a"
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
  Test != "a"
#>  [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
  Test == "A"
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
  Test != "A"
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
  Test == c("a","z")
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
  Test != c("a","z")
#>  [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
  Test 
#> 
#> Item (measurement: nominal, type: character, length = 12) 
#> 
#>  [1:12] A B C D E F A B C D E F
  Test 
#> 
#> Item (measurement: nominal, type: character, length = 12) 
#> 
#>  [1:12] A B C D E F A B C D E F

  as.factor(test)
#>  [1] a b c d e f a b c d e f
#> Levels: a b c d e f
  as.factor(Test)
#>  [1] A B C D E F A B C D E F
#> Levels: A B C D E F
  as.numeric(test)
#>  [1] 1 2 3 4 5 6 1 2 3 4 5 6
  as.character(test)
#>  [1] "a" "b" "c" "d" "e" "f" "a" "b" "c" "d" "e" "f"
  as.character(Test)
#>  [1] "A" "B" "C" "D" "E" "F" "A" "B" "C" "D" "E" "F"

  as.data.frame(test)[[1]]
#>  [1] a b c d e f a b c d e f
#> Levels: a b c d e f
```
