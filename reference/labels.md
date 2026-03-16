# Value Labels

Value labels associate character labels to possible values of an encoded
survey item. Value labels are represented as objects of class
"value.labels".

Value labels of an item can be obtained using `labels(x)` and can be
associated to items and to vectors using labels(x) \<- value

Value labels also can be updated using the `+` and `-` operators.

## Usage

``` r
labels(object,...)
labels(x) <- value
```

## Arguments

- object:

  any object.

- ...:

  further arguments for other methods.

- x:

  a vector or "item" object.

- value:

  an object of class "value.labels" or a vector that can be coerced into
  an "value.labels" object or NULL

## Examples

``` r
  x <- as.item(rep(1:5,4),
      labels=c(
          "First"      = 1,
          "Second"     = 2,
          "Third"      = 3,
          "Fourth"     = 4,
          "Don't know" = 5
        ),
      missing.values=5,
      annotation = c(
        description="test"
      ))
  labels(x)
#> 
#>  Values and labels:
#> 
#>    1 'First'     
#>    2 'Second'    
#>    3 'Third'     
#>    4 'Fourth'    
#>    5 'Don't know'
#> 
  labels(x) <- labels(x) - c("Second"=2)
  labels(x)
#> 
#>  Values and labels:
#> 
#>    1 'First'     
#>    3 'Third'     
#>    4 'Fourth'    
#>    5 'Don't know'
#> 
  labels(x) <- labels(x) + c("Second"=2)
  labels(x)
#> 
#>  Values and labels:
#> 
#>    1 'First'     
#>    2 'Second'    
#>    3 'Third'     
#>    4 'Fourth'    
#>    5 'Don't know'
#> 

  puvl <- getOption("print.use.value.labels")
  options(print.use.value.labels=FALSE)
  x
#> 
#> Item 'test' (measurement: nominal, type: integer, length = 20) 
#> 
#>  [1:20] 1 2 3 4 *5 1 2 3 4 *5 1 2 3 4 *5 1 2 3 4 *5
  options(print.use.value.labels=TRUE)
  x
#> 
#> Item 'test' (measurement: nominal, type: integer, length = 20) 
#> 
#>  [1:20] First Second Third Fourth *Don't know First Second Third Fourth ...
  options(print.use.value.labels=puvl)
```
