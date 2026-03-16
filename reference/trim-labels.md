# Trim Codes from the Labels of an Item

Occasionally, labels of codes in a survey data sets (e.g. from the 2016
American National Election Study) include a character representation of
the codes being labelled. While there may be technical reasons for this,
it is often inconvenient (e.g. if one wants to reorder the labelled
codes). The function `trim_labels` trims the code representations (if
they are present.)

## Usage

``` r
trim_labels(x,...)
# S4 method for class 'item.vector'
trim_labels(x,...)
# S4 method for class 'data.set'
trim_labels(x,...)
```

## Arguments

- x:

  An object – an "item" object or a "data.set" object

- ...:

  Further arguments, currently ignored

## Details

The "data.set" method applies the "item.vector" method to all the
labelled items in the data set.

The "item.vector" returns a copy of its argument with modified labels,
where a label such as "1. First alternative" is changed into "First
alternative".

## Examples

``` r
x <- as.item(sample(1:3,10,replace=TRUE),
             labels=c("1. One"=1,
                      "2. Two"=2,
                      "2. Three"=3))
y <- as.item(sample(1:2,10,replace=TRUE),
             labels=c("1. First category"=1,
                      "2. Second category"=2))
ds <- data.set(x,y)
x <- trim_labels(x)
codebook(x)
#> ================================================================================
#> 
#>    x
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'One'                 2    20.0
#>    2 'Two'                 3    30.0
#>    3 'Three'               5    50.0
#> 
ds <- trim_labels(ds)
codebook(ds)
#> ================================================================================
#> 
#>    x
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'One'                 2    20.0
#>    2 'Two'                 3    30.0
#>    3 'Three'               5    50.0
#> 
#> ================================================================================
#> 
#>    y
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels         N Percent
#>                                       
#>    1 'First category'        4    40.0
#>    2 'Second category'       6    60.0
#> 
```
