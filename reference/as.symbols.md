# Construction of Lists of Symbols

`as.symbols` and `syms` are functions potentially useful in connection
with `foreach` and `xapply`. `as.symbols` produces a list of symbols
from a character vector, while `syms` returns a list of symbols from
symbols given as arguments, but it can be used to construct patterns of
symbols.

## Usage

``` r
as.symbols(x)
syms(...,paste=FALSE,sep="")
```

## Arguments

- x:

  a character vector

- ...:

  character strings or (unquoted) variable names

- paste:

  logical value; should the character strings
  [`paste`](https://rdrr.io/r/base/paste.html)d into one string?

- sep:

  a separator string, passed to
  [`paste`](https://rdrr.io/r/base/paste.html).

## Value

A list of language symbols (results of
[`as.symbol`](https://rdrr.io/r/base/name.html) - not graphical
symbols!).

## Examples

``` r
  as.symbols(letters[1:8])
#> [[1]]
#> a
#> 
#> [[2]]
#> b
#> 
#> [[3]]
#> c
#> 
#> [[4]]
#> d
#> 
#> [[5]]
#> e
#> 
#> [[6]]
#> f
#> 
#> [[7]]
#> g
#> 
#> [[8]]
#> h
#> 
  syms("a",1:3,paste=TRUE)
#> [[1]]
#> a1
#> 
#> [[2]]
#> a2
#> 
#> [[3]]
#> a3
#> 

  sapply(syms("a",1:3,paste=TRUE),typeof)
#> [1] "symbol" "symbol" "symbol"
```
