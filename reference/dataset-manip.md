# Manipulation of Data Sets

Like data frames, `data.set` objects have
[`subset`](https://rdrr.io/r/base/subset.html),
[`unique`](https://rdrr.io/r/base/unique.html),
[`cbind`](https://rdrr.io/r/base/cbind.html),
[`rbind`](https://rdrr.io/r/base/cbind.html),
[`merge`](https://rdrr.io/r/base/merge.html) methods defined for them.

The semantics are basically the same as the methods defined for data
frames in the `base` package, with the only difference that the return
values are `data.set` objects. In fact, the methods described here are
front-ends to the corresponding methods for data frames, which are
constructed such that the "extra" information attached to variables
within `data.set` objects, that is, to `item` objects.

## Usage

``` r
# S3 method for class 'data.set'
subset(x, subset, select, drop = FALSE, ...)

# S4 method for class 'data.set'
unique(x, incomparables = FALSE, ...)

# S3 method for class 'data.set'
cbind(..., deparse.level = 1)

# S3 method for class 'data.set'
rbind(..., deparse.level = 1)

# S4 method for class 'data.set,data.set'
merge(x,y, ...)

# S4 method for class 'data.set,data.frame'
merge(x,y, ...)

# S4 method for class 'data.frame,data.set'
merge(x,y, ...)
```

## Arguments

- x,y:

  `data.set` objects. On of the arguments to `merge` may also be an
  object coercable into a data frame and the result still is a
  `data.set` object.

- subset:

  a logical expression, used to select observations from the data set.

- select:

  a vector with variablen names, which are retained in the data subset.

- drop:

  logical; if `TRUE` and the result has only one column, the result is
  an item and not a data set.

- ...:

  for `subset`: a logical vector of the same length as the number of
  rows of the `data.set` and, optionally, a vector of variable names
  (tagged as `select`); for `unique`: further arguments, ignored; for
  `cbind`, `rbind`: objects coercable into data frames, with at least
  one being a `data.set` object; for `merge`: further arguments such as
  arguments tagged with `by`, `by.x`, `by.y`, etc. that specify the
  variables by which to merge the data sets of data frames `x` and `y`.

- incomparables:

  a vector of values that cannot be compared. See
  [`unique`](https://rdrr.io/r/base/unique.html).

- deparse.level:

  an argument retained for reasons of compatibility of the default
  methods of [`cbind`](https://rdrr.io/r/base/cbind.html) and
  [`rbind`](https://rdrr.io/r/base/cbind.html).

## Examples

``` r
ds1 <- data.set(
      a = rep(1:3,5),
      b = rep(1:5,each=3)
  )
ds2 <- data.set(
      a = c(3:1,3,3),
      b = 1:5
  )

ds1 <- within(ds1,{
      description(a) <- "Example variable 'a'"
      description(b) <- "Example variable 'b'"
  })

ds2 <- within(ds2,{
      description(a) <- "Example variable 'a'"
      description(b) <- "Example variable 'b'"
  })

str(ds3 <- rbind(ds1,ds2))
#> Data set with 20 obs. of 2 variables:
#>  $ a: Itvl. item  num  1 2 3 1 2 3 1 2 3 1 ...
#>  $ b: Itvl. item  int  1 1 1 2 2 2 3 3 3 4 ...
description(ds3)
#> 
#>  a 'Example variable 'a''
#>  b 'Example variable 'b''
#> 

ds3 <- within(ds1,{
        c <- a
        d <- b
        description(c) <- "Copy of variable 'a'"
        description(d) <- "Copy of variable 'b'"
        rm(a,b)
    })
str(ds4 <- cbind(ds1,ds3))
#> Data set with 15 obs. of 4 variables:
#>  $ ds1.a: Itvl. item  int  1 2 3 1 2 3 1 2 3 1 ...
#>  $ ds1.b: Itvl. item  int  1 1 1 2 2 2 3 3 3 4 ...
#>  $ ds3.c: Itvl. item  int  1 2 3 1 2 3 1 2 3 1 ...
#>  $ ds3.d: Itvl. item  int  1 1 1 2 2 2 3 3 3 4 ...
description(ds4)
#> 
#>  ds1.a 'Example variable 'a''
#>  ds1.b 'Example variable 'b''
#>  ds3.c 'Copy of variable 'a''
#>  ds3.d 'Copy of variable 'b''
#> 

ds5 <- data.set(
        c = 1:3,
        d = c(1,1,2)
        )
ds5 <- within(ds5,{
      description(c) <- "Example variable 'c'"
      description(d) <- "Example variable 'd'"
  })
str(ds6 <- merge(ds1,ds5,by.x="a",by.y="c"))
#> Data set with 15 obs. of 3 variables:
#>  $ a: Itvl. item  int  1 1 1 1 1 2 2 2 2 2 ...
#>  $ b: Itvl. item  int  1 4 3 2 5 1 4 3 2 5 ...
#>  $ d: Itvl. item  num  1 1 1 1 1 1 1 1 1 1 ...

# Note that the attributes of the left-hand variables
# have priority.
description(ds6)
#> 
#>  a 'Example variable 'a''
#>  b 'Example variable 'b''
#>  d 'Example variable 'd''
#> 
```
