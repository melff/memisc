# Reverse the codes of a survey item or the levels of a factor

The function `reversed()` returns a copy of its argument with codes or
levels in reverse order.

## Usage

``` r
reversed(x)
# S4 method for class 'item.vector'
reversed(x)
# S4 method for class 'factor'
reversed(x)
```

## Arguments

- x:

  An object – an "item" object or a "data.set" object

## Value

If the argument of the function `reversed()` than either the unique
valid values or the labelled valid values recoded into the reverse
order.

If th argument is a factor than the function returns the factor with
levels in reverse order.

## Examples

``` r
ds <- data.set(
    x = as.item(sample(c(1:3,9),100,replace=TRUE),
                labels=c("One"=1,
                         "Two"=2,
                         "Three"=3,
                         "Missing"=9)))
df <- as.data.frame(ds)
ds <- within(ds,{
    xr <- reversed(x)
})
codebook(ds)
#> ================================================================================
#> 
#>    x
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'One'                26    26.0
#>    2 'Two'                22    22.0
#>    3 'Three'              28    28.0
#>    9 'Missing'            24    24.0
#> 
#> ================================================================================
#> 
#>    xr
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'Missing'            24    24.0
#>    2 'Three'              28    28.0
#>    3 'Two'                22    22.0
#>    9 'One'                26    26.0
#> 
df <- within(df,{
    xr <- reversed(x)
})
codebook(df)
#> ================================================================================
#> 
#>    x
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 4 levels
#> 
#>    Levels and labels     N Valid
#>                                 
#>    1 'One'              26  26.0
#>    2 'Two'              22  22.0
#>    3 'Three'            28  28.0
#>    4 'Missing'          24  24.0
#> 
#> ================================================================================
#> 
#>    xr
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 4 levels
#> 
#>    Levels and labels     N Valid
#>                                 
#>    1 'Missing'          24  24.0
#>    2 'Three'            28  28.0
#>    3 'Two'              22  22.0
#>    4 'One'              26  26.0
#> 
```
