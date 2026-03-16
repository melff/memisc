# Levels of Measurement of Survey Items

The measurement level of a `"item"` object, which is one of "nominal",
"ordinal", "interval", "ratio", determines what happens to it, if it or
the [`data.set`](https://melff.github.io/memisc/reference/dataSets.md)
containing it is coerced into a
[`data.frame`](https://rdrr.io/r/base/data.frame.html). If the level of
measurement level is "nominal", the it will be converted into an
(unordered) [factor](https://rdrr.io/r/base/factor.html), if the level
of measurement is "ordinal", the item will be converted into an
[ordered](https://rdrr.io/r/base/factor.html) vector. If the measurement
is "interval" or "ratio", the item will be converted into a numerical
vector.

## Usage

``` r
# S4 method for class 'item'
measurement(x)
# S4 method for class 'item'
measurement(x) <- value
# S4 method for class 'data.set'
measurement(x)
# S4 method for class 'data.set'
measurement(x) <- value
is.nominal(x)
is.ordinal(x)
is.interval(x)
is.ratio(x)
as.nominal(x)
as.ordinal(x)
as.interval(x)
as.ratio(x)
set_measurement(x,...)
```

## Arguments

- x:

  an object, usually of class `"item"`.

- value:

  for the `item` method, a character string; either "nominal",
  "ordinal", "interval", or "ratio"; for the `data.set` method, a list
  of character vectors with variable names, where the names of the list
  corresponds to a measurement level and and the list elements indicates
  the variables to which the measurement levels are assigned.

- ...:

  vectors of variable names, either symbols or character strings, tagged
  with the intended measurement level.

## Value

The `item` method of `measurement(x)` returns a character string, the
`data.set` method returns a named character vector, where the name of
each element is a variable name and each.

`as.nominal`, `as.ordinal`, `as.interval`, `as.ratio` return an item
with the requested level of measurement setting.

`is.nominal`, `is.ordinal`, `is.interval`, `is.ratio` return a logical
value.

## References

Stevens, Stanley S. 1946. "On the theory of scales of measurement."
*Science* 103: 677-680.

## See also

[`data.set`](https://melff.github.io/memisc/reference/dataSets.md),
[`item`](https://melff.github.io/memisc/reference/items.md)

## Examples

``` r
vote <- sample(c(1,2,3,8,9),size=30,replace=TRUE)
labels(vote) <- c(Conservatives         =  1,
                  Labour                =  2,
                  "Liberal Democrats"   =  3,
                  "Don't know"          =  8,
                  "Answer refused"      =  9
                  )
missing.values(vote) <- c(8,9)
as.data.frame(vote)[[1]]
#>  [1] Labour            <NA>              Liberal Democrats <NA>             
#>  [5] Conservatives     Liberal Democrats <NA>              <NA>             
#>  [9] Liberal Democrats Labour            <NA>              Liberal Democrats
#> [13] Conservatives     Labour            Labour            <NA>             
#> [17] <NA>              <NA>              Liberal Democrats Conservatives    
#> [21] Conservatives     Conservatives     <NA>              Labour           
#> [25] Conservatives     <NA>              <NA>              Labour           
#> [29] Liberal Democrats <NA>             
#> Levels: Conservatives Labour Liberal Democrats
measurement(vote) <- "interval"
as.data.frame(vote)[[1]]
#>  [1]  2 NA  3 NA  1  3 NA NA  3  2 NA  3  1  2  2 NA NA NA  3  1  1  1 NA  2  1
#> [26] NA NA  2  3 NA
vote <- as.nominal(vote)
as.data.frame(vote)[[1]]
#>  [1] Labour            <NA>              Liberal Democrats <NA>             
#>  [5] Conservatives     Liberal Democrats <NA>              <NA>             
#>  [9] Liberal Democrats Labour            <NA>              Liberal Democrats
#> [13] Conservatives     Labour            Labour            <NA>             
#> [17] <NA>              <NA>              Liberal Democrats Conservatives    
#> [21] Conservatives     Conservatives     <NA>              Labour           
#> [25] Conservatives     <NA>              <NA>              Labour           
#> [29] Liberal Democrats <NA>             
#> Levels: Conservatives Labour Liberal Democrats
group <- sample(c(1,2),size=30,replace=TRUE)
labels(group) <- c(A=1,B=2)
DataS <- data.set(group,vote)
measurement(DataS)
#> NULL
measurement(DataS) <- list(interval=c("group","vote"))
head(as.data.frame(DataS))
#>   group vote
#> 1     2    2
#> 2     1   NA
#> 3     2    3
#> 4     1   NA
#> 5     1    1
#> 6     1    3
DataS <- set_measurement(DataS,
                         nominal=c(group,vote))
head(as.data.frame(DataS))
#>   group              vote
#> 1     B            Labour
#> 2     A              <NA>
#> 3     B Liberal Democrats
#> 4     A              <NA>
#> 5     A     Conservatives
#> 6     A Liberal Democrats
```
