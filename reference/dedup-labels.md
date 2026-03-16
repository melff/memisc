# Handle duplicated labels

The function `deduplicate_labels` can be used with "item" objects,
"importer" objects or "data.set" objects to deal with duplicate labels,
i.e. labels that are attached to more than one code. There are several
ways to de-duplicate labels: by combining values that share their label
or by making labels duplicate labels distinct.

## Usage

``` r
deduplicate_labels(x,...)
# S3 method for class 'item'
deduplicate_labels(x,
    method=c("combine codes",
             "prefix values",
             "postfix values"),...)
# Applicable to 'importer' objects and 'data.set' objects
# S3 method for class 'item.list'
deduplicate_labels(x,...)
```

## Arguments

- x:

  an item with value labels or that contains items with value labels

- method:

  a character string that determines the method to make value labels
  unique.

- ...:

  other arguments, passed to specific methods of the generic function.

## Value

The function `deduplicate_labels` a copy of `x` that has unqiue value
labels.

## Examples

``` r
x1 <- as.item(rep(1:5,4),
              labels=c(
                  A = 1,
                  A = 2,
                  B = 3,
                  B = 4,
                  C = 5
              ),
              annotation = c(
                  description="Yet another test"
))
#> Warning: Duplicate labels 'A' 'B'
              
x2 <- as.item(rep(1:4,5),
              labels=c(
                  i   = 1,
                  ii  = 2,
                  iii = 3,
                  iii = 4
                  ),
              annotation = c(
                  description="Still another test"
))
#> Warning: Duplicate labels 'iii'

x3 <- as.item(rep(1:2,10),
              labels=c(
                  a = 1,
                  b = 2
                  ),
              annotation = c(
                  description="Still another test"
))
                            
codebook(deduplicate_labels(x1))
#> ================================================================================
#> 
#>    deduplicate_labels(x1) 'Yet another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   8    40.0
#>    3 'B'                   8    40.0
#>    5 'C'                   4    20.0
#> 
codebook(deduplicate_labels(x1,method="prefix"))
#> ================================================================================
#> 
#>    deduplicate_labels(x1, method = "prefix") 'Yet another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 '1. A'                4    20.0
#>    2 '2. A'                4    20.0
#>    3 '3. B'                4    20.0
#>    4 '4. B'                4    20.0
#>    5 '5. C'                4    20.0
#> 
codebook(deduplicate_labels(x1,method="postfix"))
#> ================================================================================
#> 
#>    deduplicate_labels(x1, method = "postfix") 'Yet another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A (1)'               4    20.0
#>    2 'A (2)'               4    20.0
#>    3 'B (3)'               4    20.0
#>    4 'B (4)'               4    20.0
#>    5 'C'                   4    20.0
#> 

ds <- data.set(x1,x2,x3)
codebook(deduplicate_labels(ds))
#> ================================================================================
#> 
#>    x1 'Yet another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A'                   8    40.0
#>    3 'B'                   8    40.0
#>    5 'C'                   4    20.0
#> 
#> ================================================================================
#> 
#>    x2 'Still another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'i'                   5    25.0
#>    2 'ii'                  5    25.0
#>    3 'iii'                10    50.0
#> 
#> ================================================================================
#> 
#>    x3 'Still another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                  10    50.0
#>    2 'b'                  10    50.0
#> 
codebook(deduplicate_labels(ds,method="prefix"))
#> ================================================================================
#> 
#>    x1 'Yet another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 '1. A'                4    20.0
#>    2 '2. A'                4    20.0
#>    3 '3. B'                4    20.0
#>    4 '4. B'                4    20.0
#>    5 '5. C'                4    20.0
#> 
#> ================================================================================
#> 
#>    x2 'Still another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 '1. i'                5    25.0
#>    2 '2. ii'               5    25.0
#>    3 '3. iii'              5    25.0
#>    4 '4. iii'              5    25.0
#> 
#> ================================================================================
#> 
#>    x3 'Still another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                  10    50.0
#>    2 'b'                  10    50.0
#> 
codebook(deduplicate_labels(ds,method="postfix"))
#> ================================================================================
#> 
#>    x1 'Yet another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'A (1)'               4    20.0
#>    2 'A (2)'               4    20.0
#>    3 'B (3)'               4    20.0
#>    4 'B (4)'               4    20.0
#>    5 'C'                   4    20.0
#> 
#> ================================================================================
#> 
#>    x2 'Still another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'i'                   5    25.0
#>    2 'ii'                  5    25.0
#>    3 'iii (3)'             5    25.0
#>    4 'iii (4)'             5    25.0
#> 
#> ================================================================================
#> 
#>    x3 'Still another test'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: nominal
#> 
#>    Values and labels       N Percent
#>                                     
#>    1 'a'                  10    50.0
#>    2 'b'                  10    50.0
#> 
```
