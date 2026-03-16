# Negative Match

`%nin%` is a convenience operator: `x %nin% table` is equivalent to
`!(x %in% table).`

## Usage

``` r
x %nin% table
```

## Arguments

- x:

  the values to be matched

- table:

  a values to be match against

## Value

A logical vector

## Examples

``` r
x <- sample(1:6,12,replace=TRUE)
x %in% 1:3
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE FALSE
x %nin% 1:3
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
```
