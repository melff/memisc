# Table of frequencies for unlabelled codes

The function `wild.codes` creates a table of frequencies of those codes
of an item that do not have labelled attached to them. This way, it
helps to identify coding errors.

## Usage

``` r
wild.codes(x)
# S4 method for class 'item'
wild.codes(x)
```

## Arguments

- x:

  an object of class "item"

## Value

A table of frequencies (i.e. an array of class "table")
