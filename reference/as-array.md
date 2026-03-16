# Converting Data Frames into Arrays

The `as.array` for data frames takes all factors in a data frame and
uses them to define the dimensions of the resulting array, and fills the
array with the values of the remaining numeric variables.

Currently, the data frame must contain all combinations of factor
levels.

## Usage

``` r
# S4 method for class 'data.frame'
as.array(x,data.name=NULL,...)
```

## Arguments

- x:

  a data frame

- data.name:

  a character string, giving the name attached to the dimension that
  corresponds to the numerical variables in the data frame (that is, the
  `name` attached to the corresponding element of the `dimnames` list).

- ...:

  other arguments, ignored.

## Value

An array

## Examples

``` r
BerkeleyAdmissions <- to.data.frame(UCBAdmissions)
BerkeleyAdmissions
#>    Gender Dept Admitted Rejected
#> 1    Male    A      512      313
#> 2  Female    A       89       19
#> 3    Male    B      353      207
#> 4  Female    B       17        8
#> 5    Male    C      120      205
#> 6  Female    C      202      391
#> 7    Male    D      138      279
#> 8  Female    D      131      244
#> 9    Male    E       53      138
#> 10 Female    E       94      299
#> 11   Male    F       22      351
#> 12 Female    F       24      317
as.array(BerkeleyAdmissions,data.name="Admit")
#> , , Dept = A
#> 
#>           Gender
#> Admit      Male Female
#>   Admitted  512     89
#>   Rejected  313     19
#> 
#> , , Dept = B
#> 
#>           Gender
#> Admit      Male Female
#>   Admitted  353     17
#>   Rejected  207      8
#> 
#> , , Dept = C
#> 
#>           Gender
#> Admit      Male Female
#>   Admitted  120    202
#>   Rejected  205    391
#> 
#> , , Dept = D
#> 
#>           Gender
#> Admit      Male Female
#>   Admitted  138    131
#>   Rejected  279    244
#> 
#> , , Dept = E
#> 
#>           Gender
#> Admit      Male Female
#>   Admitted   53     94
#>   Rejected  138    299
#> 
#> , , Dept = F
#> 
#>           Gender
#> Admit      Male Female
#>   Admitted   22     24
#>   Rejected  351    317
#> 
try(as.array(BerkeleyAdmissions[-1,],data.name="Admit"))
#> Error : cannot transform 'x' into an array
```
