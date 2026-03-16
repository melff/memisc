# Mark Negative Values as Missing

In many newer survey data sets available from social science data
archives non-valid responses (such as "don't know" or "answer refused")
are given negative codes. The function `neg2miss` allows to mark them as
missing values.)

## Usage

``` r
neg2mis(x,all=FALSE,exclude=NULL,select=NULL,zero=FALSE)
```

## Arguments

- x:

  an object that inherits from class "item.list", e.g. a "data.set" or
  an "importer" object.

- all:

  logical; should the marking of negative values as missing applied to
  all variables?

- exclude:

  an optional vector of variable naems to which the marking of negative
  values as missing should *not* be applied.

- select:

  an optional vector of variable names to which the marking of negative
  values as missing should be applied.

- zero:

  logical; should zeroes also be marked as missing?

## Examples

``` r
ds <- data.set(
          var1 = c(0,1,-1,2,3),
          var2 = c(-1,-1,1,1,1),
          var3 = c(1,2,3,4,5)
          )
neg2mis(ds,all=TRUE)
#> 
#> Data set with 5 observations and 3 variables
#> 
#>   var1 var2 var3
#> 1    0  *-1    1
#> 2    1  *-1    2
#> 3  *-1    1    3
#> 4    2    1    4
#> 5    3    1    5
neg2mis(ds,all=TRUE,zero=TRUE)
#> 
#> Data set with 5 observations and 3 variables
#> 
#>   var1 var2 var3
#> 1   *0  *-1    1
#> 2    1  *-1    2
#> 3  *-1    1    3
#> 4    2    1    4
#> 5    3    1    5
neg2mis(ds,exclude=var1)
#> 
#> Data set with 5 observations and 3 variables
#> 
#>   var1 var2 var3
#> 1    0  *-1    1
#> 2    1  *-1    2
#> 3   -1    1    3
#> 4    2    1    4
#> 5    3    1    5
neg2mis(ds,select=var1)
#> 
#> Data set with 5 observations and 3 variables
#> 
#>   var1 var2 var3
#> 1    0   -1    1
#> 2    1   -1    2
#> 3  *-1    1    3
#> 4    2    1    4
#> 5    3    1    5
```
