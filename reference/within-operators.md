# Operators to abbreviate use of "with" and "within"

The operators `%$%` and `%$$%` provide abbrevitions for calls to
[`with()`](https://rdrr.io/r/base/with.html) and
[`within()`](https://rdrr.io/r/base/with.html) respectively. The
function `Within()` is a variant of
[`with()`](https://rdrr.io/r/base/with.html) were the resulting data
frame contains any newly created variables in the order in which they
are created (and not in the reverse order).

## Usage

``` r
data %$% expr
data %$$% expr
Within(data,expr,...)
# S3 method for class 'data.frame'
Within(data,expr,...)
```

## Arguments

- data:

  a data frame or similar object, see
  [`with`](https://rdrr.io/r/base/with.html) and
  [`within`](https://rdrr.io/r/base/with.html)

- expr:

  a single or compound expression (i.e. several expressions enclosed in
  curly braces), see [`with`](https://rdrr.io/r/base/with.html) and
  [`within`](https://rdrr.io/r/base/with.html)

- ...:

  Further arguments, currently ignored

## See also

[`with`](https://rdrr.io/r/base/with.html) and
[`within`](https://rdrr.io/r/base/with.html) in package "base".

## Examples

``` r
df <- data.frame(a = 1:7,
                 b = 7:1)

df
#>   a b
#> 1 1 7
#> 2 2 6
#> 3 3 5
#> 4 4 4
#> 5 5 3
#> 6 6 2
#> 7 7 1

df <- within(df,{
  ab <- a + b
  a2b2 <- a^2 + b^2
})
df
#>   a b a2b2 ab
#> 1 1 7   50  8
#> 2 2 6   40  8
#> 3 3 5   34  8
#> 4 4 4   32  8
#> 5 5 3   34  8
#> 6 6 2   40  8
#> 7 7 1   50  8

df <- data.frame(a = 1:7,
                 b = 7:1)
df <- Within(df,{
  ab <- a + b
  a2b2 <- a^2 + b^2
})
df
#>   a b ab a2b2
#> 1 1 7  8   50
#> 2 2 6  8   40
#> 3 3 5  8   34
#> 4 4 4  8   32
#> 5 5 3  8   34
#> 6 6 2  8   40
#> 7 7 1  8   50


df <- data.frame(a = 1:7,
                 b = 7:1)
df
#>   a b
#> 1 1 7
#> 2 2 6
#> 3 3 5
#> 4 4 4
#> 5 5 3
#> 6 6 2
#> 7 7 1

ds <- as.data.set(df)
ds
#> 
#> Data set with 7 observations and 2 variables
#> 
#>   a b
#> 1 1 7
#> 2 2 6
#> 3 3 5
#> 4 4 4
#> 5 5 3
#> 6 6 2
#> 7 7 1

df %$$% {
  ab <- a + b
  a2b2 <- a^2 + b^2
}
df
#>   a b ab a2b2
#> 1 1 7  8   50
#> 2 2 6  8   40
#> 3 3 5  8   34
#> 4 4 4  8   32
#> 5 5 3  8   34
#> 6 6 2  8   40
#> 7 7 1  8   50

ds %$$% {
  ab <- a + b
  a2b2 <- a^2 + b^2
}
#> Note: method with signature ‘numeric.item#numeric’ chosen for function ‘+’,
#>  target signature ‘integer.item#integer.item’.
#>  "numeric#numeric.item" would also be valid
ds
#> 
#> Data set with 7 observations and 4 variables
#> 
#>   a b ab a2b2
#> 1 1 7  8   50
#> 2 2 6  8   40
#> 3 3 5  8   34
#> 4 4 4  8   32
#> 5 5 3  8   34
#> 6 6 2  8   40
#> 7 7 1  8   50

df %$% c(a.ssq = sum(a^2),
         b.ssq = sum(b^2))
#> a.ssq b.ssq 
#>   140   140 
```
