# Automatically Adapt Measurement Levels

The generic function `measurement_autolevel` changes the measurement
levels of "item" objects to "nominal" or "ordinal", if the proportion of
its values that have labels is above a certain threshold.

## Usage

``` r
measurement_autolevel(x, ...)
# S4 method for class 'ANY'
measurement_autolevel(x, ...) # Returns its argument as is
# S4 method for class 'item.vector'
measurement_autolevel(x, 
                to=getOption("measurement.adapt.default","nominal"),
                threshold=getOption("measurement.adapt.threshold",.75),
                ...)
# S4 method for class 'data.set'
measurement_autolevel(x, 
                to=getOption("measurement.adapt.default","nominal"),
                threshold=getOption("measurement.adapt.threshold",.75),
                except=NULL,
                only=NULL,
                ...)
```

## Arguments

- x:

  an object from class "item.vector" or "data.set".

- to:

  a character vector, the target measurement level

- threshold:

  the proportion of values, if reached the target measurement level is
  set

- except:

  a vector with variable names, either as symbols (without quotation
  marks) or character strings (with quotation markes), the variables in
  the data set that are not to be changed by `measurement_autolevel()`.

- only:

  a vector with variable names, either as symbols (without quotation
  marks) or character strings (with quotation markes), the variables in
  the data set that are to be changed by `measurement_autolevel()`.

- ...:

  other arguments, currently ignored.

## Examples

``` r
 exvect <- as.item(rep(1:2,5))
 labels(exvect) <- c(a=1,b=2)
 codebook(exvect)
#> ================================================================================
#> 
#>    exvect
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   5    50.0
#>    2 'b'                   5    50.0
#>                                     
#>         Min: 1.000                  
#>         Max: 2.000                  
#>        Mean: 1.500                  
#>    Std.Dev.: 0.500                  
#> 
 codebook(measurement_autolevel(exvect))
#> ================================================================================
#> 
#>    measurement_autolevel(exvect)
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   5    50.0
#>    2 'b'                   5    50.0
#> 

 avect <- as.item(sample(1:3,16,replace=TRUE))
 labels(avect) <- c(a=1,b=2,c=3)
 bvect <- as.item(sample(1:4,16,replace=TRUE))
 labels(bvect) <- c(A=1,B=2,C=3,D=4)
 ds <- data.set(a=avect,b=bvect)
 codebook(ds)
#> ================================================================================
#> 
#>    a
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   3    18.8
#>    2 'b'                   4    25.0
#>    3 'c'                   9    56.2
#>                                     
#>         Min: 1.000                  
#>         Max: 3.000                  
#>        Mean: 2.375                  
#>    Std.Dev.: 0.781                  
#> 
#> ================================================================================
#> 
#>    b
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   7    43.8
#>    2 'B'                   5    31.2
#>    3 'C'                   2    12.5
#>    4 'D'                   2    12.5
#>                                     
#>         Min: 1.000                  
#>         Max: 4.000                  
#>        Mean: 1.938                  
#>    Std.Dev.: 1.029                  
#> 
 codebook(measurement_autolevel(ds))
#> ================================================================================
#> 
#>    a
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   3    18.8
#>    2 'b'                   4    25.0
#>    3 'c'                   9    56.2
#> 
#> ================================================================================
#> 
#>    b
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   7    43.8
#>    2 'B'                   5    31.2
#>    3 'C'                   2    12.5
#>    4 'D'                   2    12.5
#> 
 codebook(measurement_autolevel(ds,except=c(a,b)))
#> ================================================================================
#> 
#>    a
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   3    18.8
#>    2 'b'                   4    25.0
#>    3 'c'                   9    56.2
#>                                     
#>         Min: 1.000                  
#>         Max: 3.000                  
#>        Mean: 2.375                  
#>    Std.Dev.: 0.781                  
#> 
#> ================================================================================
#> 
#>    b
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   7    43.8
#>    2 'B'                   5    31.2
#>    3 'C'                   2    12.5
#>    4 'D'                   2    12.5
#>                                     
#>         Min: 1.000                  
#>         Max: 4.000                  
#>        Mean: 1.938                  
#>    Std.Dev.: 1.029                  
#> 
 codebook(measurement_autolevel(ds,only=a))
#> ================================================================================
#> 
#>    a
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                   3    18.8
#>    2 'b'                   4    25.0
#>    3 'c'                   9    56.2
#> 
#> ================================================================================
#> 
#>    b
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: interval
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   7    43.8
#>    2 'B'                   5    31.2
#>    3 'C'                   2    12.5
#>    4 'D'                   2    12.5
#>                                     
#>         Min: 1.000                  
#>         Max: 4.000                  
#>        Mean: 1.938                  
#>    Std.Dev.: 1.029                  
#> 
```
