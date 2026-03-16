# Substitutions in Language Objects

`Substitute` differs from
[`substitute`](https://rdrr.io/r/base/substitute.html) in so far as its
first argument can be a variable that contains an object of mode
"language". In that case, substitutions take place inside this object.

## Usage

``` r
Substitute(lang,with)
```

## Arguments

- lang:

  any object, unevaluated expression, or unevaluated language construct,
  such as a sequence of calls inside braces

- with:

  a named list, environment, data frame or data set.

## Details

The function body is just `do.call("substitute",list(lang,with))`.

## Value

An object of storage mode "language" or "symbol".

## Examples

``` r
lang <- quote(sin(x)+z)
substitute(lang,list(x=1,z=2))
#> lang
Substitute(lang,list(x=1,z=2))
#> sin(1) + 2
```
