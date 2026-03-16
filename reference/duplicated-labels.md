# Check for and report duplicated labels

The function `duplicated_labels` can be used with "item" objects,
"importer" objects or "data.set" objects to check whether items contain
duplicate labels, i.e. labels that are attached to more than one code.

## Usage

``` r
duplicated_labels(x)
# S3 method for class 'item'
duplicated_labels(x)
# Applicable to 'importer' objects and 'data.set' objects
# S3 method for class 'item.list'
duplicated_labels(x)
```

## Arguments

- x:

  an item with value labels or that contains items with value labels

## Value

The function `duplicate.labels` returns a list with a class attribute,
which allows pretty printing of duplicated value labels

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
                            
duplicated_labels(x1)
#> A: 1, 2
#> B: 3, 4
ds <- data.set(x1,x2,x3)
duplicated_labels(ds)
#> 
#> ================================================================================
#>  x1: 'Yet another test'
#> --------------------------------------------------------------------------------
#>   A: 1, 2
#>   B: 3, 4
#> 
#> ================================================================================
#>  x2: 'Still another test'
#> --------------------------------------------------------------------------------
#>   iii: 3, 4
codebook(ds)
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
#>    1 'A'                   4    20.0
#>    2 'A'                   4    20.0
#>    3 'B'                   4    20.0
#>    4 'B'                   4    20.0
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
#>    3 'iii'                 5    25.0
#>    4 'iii'                 5    25.0
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

nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())
nes1948 <- spss.portable.file(nes1948.por)
#> Warning: 9 variables have duplicated labels:
#>   V480004, V480012, V480020, V480021A, V480021B, V480033A, V480033B,
#>   V480034A, V480034B
duplicated_labels(nes1948)             
#> 
#> ================================================================================
#>  V480004: 'CODER'
#> --------------------------------------------------------------------------------
#>   NAME NOT KNOWN: 1, 2, 3, 4, 5
#> 
#> ================================================================================
#>  V480012: 'WHAT TO DO W TFT-HT ACT'
#> --------------------------------------------------------------------------------
#>   HAVE HEARD OF TAFT-HARTLEY ACT: 5, 6
#> 
#> ================================================================================
#>  V480020: 'CNSD VT FOR SOMEONE ELSE'
#> --------------------------------------------------------------------------------
#>   DID NOT CONSIDER ANYONE ELSE: 11, 22, 33, 44
#>   CONSIDERED WALLACE:           13, 23        
#>   CONSIDERED OTHER:             14, 24        
#>   NA:                           19, 29, 49    
#>   CONSIDERED TRUMAN:            21, 41        
#> 
#> ================================================================================
#>  V480021A: 'XWHY DID NOT VT FOR HIM 1'
#> --------------------------------------------------------------------------------
#>   DISAGREED WITH PLATFORM OR POLICY - TO: 3, 4
#> 
#> ================================================================================
#>  V480021B: 'XWHY DID NOT VT FOR HIM 2'
#> --------------------------------------------------------------------------------
#>   DISAGREED WITH PLATFORM OR POLICY - TO: 3, 4
#> 
#> ================================================================================
#>  V480033A: 'ISSUES CONNECTED W TRMN 1'
#> --------------------------------------------------------------------------------
#>   RENT CONTROL:              1, 16, 31, 51, 66, 81 
#>   PRICE CONTROL:             2, 17, 32, 52, 67, 82 
#>   TAFT-HARTLEY:              3, 18, 33, 53, 68, 83 
#>   FARM PRICES AND SUPPORT:   4, 19, 34, 54, 69, 84 
#>   LOWER INCOME TAX:          5, 20, 35, 55, 70, 85 
#>   CIVIL RIGHTS:              6, 21, 36, 56, 71, 86 
#>   BALANCE BUDGET:            7, 22, 37, 57, 72, 87 
#>   (GOVERNMENT) HOUSING:      8, 23, 38, 58, 73, 88 
#>   DEFENSE ACTIVITY:          9, 24, 39, 59, 74, 89 
#>   GOVERNMENT ATOMIC CONTROL: 10, 25, 40, 60, 75, 90
#>   NEW DEAL:                  11, 26, 41, 61, 76, 91
#>   MARSHALL PLAN:             12, 27, 42, 62, 77, 92
#>   FIRM RUSSIAN POLICY:       13, 28, 43, 63, 78, 93
#>   HELP ISRAEL (PALESTINE):   14, 29, 44, 64, 79, 94
#>   PROMOTE PEACE:             15, 30, 45, 65, 80, 95
#> 
#> ================================================================================
#>  V480033B: 'ISSUES CONNECTED W TRMN 2'
#> --------------------------------------------------------------------------------
#>   RENT CONTROL:              1, 16, 31, 51, 66, 81 
#>   PRICE CONTROL:             2, 17, 32, 52, 67, 82 
#>   TAFT-HARTLEY:              3, 18, 33, 53, 68, 83 
#>   FARM PRICES AND SUPPORT:   4, 19, 34, 54, 69, 84 
#>   LOWER INCOME TAX:          5, 20, 35, 55, 70, 85 
#>   CIVIL RIGHTS:              6, 21, 36, 56, 71, 86 
#>   BALANCE BUDGET:            7, 22, 37, 57, 72, 87 
#>   (GOVERNMENT) HOUSING:      8, 23, 38, 58, 73, 88 
#>   DEFENSE ACTIVITY:          9, 24, 39, 59, 74, 89 
#>   GOVERNMENT ATOMIC CONTROL: 10, 25, 40, 60, 75, 90
#>   NEW DEAL:                  11, 26, 41, 61, 76, 91
#>   MARSHALL PLAN:             12, 27, 42, 62, 77, 92
#>   FIRM RUSSIAN POLICY:       13, 28, 43, 63, 78, 93
#>   HELP ISRAEL (PALESTINE):   14, 29, 44, 64, 79, 94
#>   PROMOTE PEACE:             15, 30, 45, 65, 80, 95
#> 
#> ================================================================================
#>  V480034A: 'ISSUES CONNECTED W DEWEY 1'
#> --------------------------------------------------------------------------------
#>   RENT CONTROL:              1, 16, 31, 51, 66, 81 
#>   PRICE CONTROL:             2, 17, 32, 52, 67, 82 
#>   TAFT-HARTLEY:              3, 18, 33, 53, 68, 83 
#>   FARM PRICES AND SUPPORT:   4, 19, 34, 54, 69, 84 
#>   LOWER INCOME TAX:          5, 20, 35, 55, 70, 85 
#>   CIVIL RIGHTS:              6, 21, 36, 56, 71, 86 
#>   BALANCE BUDGET:            7, 22, 37, 57, 72, 87 
#>   (GOVERNMENT) HOUSING:      8, 23, 38, 58, 73, 88 
#>   DEFENSE ACTIVITY:          9, 24, 39, 59, 74, 89 
#>   GOVERNMENT ATOMIC CONTROL: 10, 25, 40, 60, 75, 90
#>   NEW DEAL:                  11, 26, 41, 61, 76, 91
#>   MARSHALL PLAN:             12, 27, 42, 62, 77, 92
#>   FIRM RUSSIAN POLICY:       13, 28, 43, 63, 78, 93
#>   HELP ISRAEL (PALESTINE):   14, 29, 44, 64, 79, 94
#>   PROMOTE PEACE:             15, 30, 45, 65, 80, 95
#> 
#> ================================================================================
#>  V480034B: 'ISSUES CONNECTED W DEWEY 2'
#> --------------------------------------------------------------------------------
#>   RENT CONTROL:              1, 16, 31, 51, 66, 81 
#>   PRICE CONTROL:             2, 17, 32, 52, 67, 82 
#>   TAFT-HARTLEY:              3, 18, 33, 53, 68, 83 
#>   FARM PRICES AND SUPPORT:   4, 19, 34, 54, 69, 84 
#>   LOWER INCOME TAX:          5, 20, 35, 55, 70, 85 
#>   CIVIL RIGHTS:              6, 21, 36, 56, 71, 86 
#>   BALANCE BUDGET:            7, 22, 37, 57, 72, 87 
#>   (GOVERNMENT) HOUSING:      8, 23, 38, 58, 73, 88 
#>   DEFENSE ACTIVITY:          9, 24, 39, 59, 74, 89 
#>   GOVERNMENT ATOMIC CONTROL: 10, 25, 40, 60, 75, 90
#>   NEW DEAL:                  11, 26, 41, 61, 76, 91
#>   MARSHALL PLAN:             12, 27, 42, 62, 77, 92
#>   FIRM RUSSIAN POLICY:       13, 28, 43, 63, 78, 93
#>   HELP ISRAEL (PALESTINE):   14, 29, 44, 64, 79, 94
#>   PROMOTE PEACE:             15, 30, 45, 65, 80, 95
```
