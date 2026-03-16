# Contract data into pattern-frequency format

`contract()` contracts data into pattern-frequency format, similar to a
contatenation of [`table()`](https://rdrr.io/r/base/table.html) (or
`xtabs`) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html). Yet it
uses much less memory if patterns are sparse, because it does not create
rows for patterns that do not occur.

## Usage

``` r
contract(x,...)
# S3 method for class 'data.frame'
contract(x,by=NULL, weights=NULL,name="Freq",
    force.name=FALSE,sort=FALSE,drop.na=TRUE,...)
# S3 method for class 'data.set'
contract(x,by=NULL, weights=NULL,name="Freq",
    force.name=FALSE,sort=FALSE,drop.na=TRUE,...)
```

## Arguments

- x:

  an object of class `"data.frame"` or `"data.set"`.

- by:

  the formula or a vector of variable names (quoted or not quoted).
  Specifies the patterns (and optionally weights). If `by` is a formula,
  then the right-hand side specifies the variables the value patterns of
  which are counted. If the left-hand side of the formula is (the name
  of) a numeric vector, its values are used as weights (in which case
  the `weights` argument will be ignored.) If the left-hand side of the
  formula is (the name of) a factor, counts are computed in separate
  columns for each of its levels.

- weights:

  a numeric vector of weights or `NULL`.

- name:

  a character string, the name of the variable that containts the
  frequency counts of the value patterns.

- force.name:

  a logical value, defaults to `FALSE`. If `TRUE` and the left-hand side
  of `by` formula is a factor, the names of the columns with the counts
  are combinations of the labels of the factor levels and the argument
  of `name`; if `FALSE`, the column names are created from the labels of
  the factor levels only.

- sort:

  a logical value, defaults to `FALSE`. If `TRUE`, the resulting data
  set is sorted by the variables that define the patterns. If `FALSE`,
  the row of the resulting data frame or data set are ordered according
  to the occurrence of the patterns.

- drop.na:

  a logical value, defaults to `TRUE`. If `FALSE`, patterns that involve
  `NA` are included in the resulting data frame or data set.

- ...:

  further arguments, passed to methods or ignored.

## Value

If `x` is a data fame, the value of `contract()` is also a data frame.
If it is a `"data.set"` object, the result is also a `"data.set"`
object.

## Examples

``` r
iris_ <- sample(iris,size=nrow(iris),replace=TRUE)
w <- rep(1,nrow(iris_))
contract(iris[4:5])
#>    Petal.Width    Species Freq
#> 1          0.2     setosa   29
#> 2          0.4     setosa    7
#> 3          0.3     setosa    7
#> 4          0.1     setosa    5
#> 5          0.5     setosa    1
#> 6          0.6     setosa    1
#> 7          1.4 versicolor    7
#> 8          1.5 versicolor   10
#> 9          1.3 versicolor   13
#> 10         1.6 versicolor    3
#> 11         1.0 versicolor    7
#> 12         1.1 versicolor    3
#> 13         1.8 versicolor    1
#> 14         1.2 versicolor    5
#> 15         1.7 versicolor    1
#> 16         2.5  virginica    3
#> 17         1.9  virginica    5
#> 18         2.1  virginica    6
#> 19         1.8  virginica   11
#> 20         2.2  virginica    3
#> 21         1.7  virginica    1
#> 22         2.0  virginica    6
#> 23         2.4  virginica    3
#> 24         2.3  virginica    8
#> 25         1.5  virginica    2
#> 26         1.6  virginica    1
#> 27         1.4  virginica    1
contract(iris[4:5],sort=TRUE)
#>    Petal.Width    Species Freq
#> 1          0.1     setosa    5
#> 2          0.2     setosa   29
#> 3          0.3     setosa    7
#> 4          0.4     setosa    7
#> 5          0.5     setosa    1
#> 6          0.6     setosa    1
#> 7          1.0 versicolor    7
#> 8          1.1 versicolor    3
#> 9          1.2 versicolor    5
#> 10         1.3 versicolor   13
#> 11         1.4 versicolor    7
#> 12         1.5 versicolor   10
#> 13         1.6 versicolor    3
#> 14         1.7 versicolor    1
#> 15         1.8 versicolor    1
#> 16         1.4  virginica    1
#> 17         1.5  virginica    2
#> 18         1.6  virginica    1
#> 19         1.7  virginica    1
#> 20         1.8  virginica   11
#> 21         1.9  virginica    5
#> 22         2.0  virginica    6
#> 23         2.1  virginica    6
#> 24         2.2  virginica    3
#> 25         2.3  virginica    8
#> 26         2.4  virginica    3
#> 27         2.5  virginica    3
contract(iris[4:5],weights=w,sort=TRUE)
#>    Petal.Width    Species Freq
#> 1          0.1     setosa    5
#> 2          0.2     setosa   29
#> 3          0.3     setosa    7
#> 4          0.4     setosa    7
#> 5          0.5     setosa    1
#> 6          0.6     setosa    1
#> 7          1.0 versicolor    7
#> 8          1.1 versicolor    3
#> 9          1.2 versicolor    5
#> 10         1.3 versicolor   13
#> 11         1.4 versicolor    7
#> 12         1.5 versicolor   10
#> 13         1.6 versicolor    3
#> 14         1.7 versicolor    1
#> 15         1.8 versicolor    1
#> 16         1.4  virginica    1
#> 17         1.5  virginica    2
#> 18         1.6  virginica    1
#> 19         1.7  virginica    1
#> 20         1.8  virginica   11
#> 21         1.9  virginica    5
#> 22         2.0  virginica    6
#> 23         2.1  virginica    6
#> 24         2.2  virginica    3
#> 25         2.3  virginica    8
#> 26         2.4  virginica    3
#> 27         2.5  virginica    3
contract(iris,by=c(Petal.Width,Species),sort=TRUE)
#>    Petal.Width    Species Freq
#> 1          0.1     setosa    5
#> 2          0.2     setosa   29
#> 3          0.3     setosa    7
#> 4          0.4     setosa    7
#> 5          0.5     setosa    1
#> 6          0.6     setosa    1
#> 7          1.0 versicolor    7
#> 8          1.1 versicolor    3
#> 9          1.2 versicolor    5
#> 10         1.3 versicolor   13
#> 11         1.4 versicolor    7
#> 12         1.5 versicolor   10
#> 13         1.6 versicolor    3
#> 14         1.7 versicolor    1
#> 15         1.8 versicolor    1
#> 16         1.4  virginica    1
#> 17         1.5  virginica    2
#> 18         1.6  virginica    1
#> 19         1.7  virginica    1
#> 20         1.8  virginica   11
#> 21         1.9  virginica    5
#> 22         2.0  virginica    6
#> 23         2.1  virginica    6
#> 24         2.2  virginica    3
#> 25         2.3  virginica    8
#> 26         2.4  virginica    3
#> 27         2.5  virginica    3
contract(iris,by=~Petal.Width+Species)
#>    Petal.Width    Species Freq
#> 1          0.2     setosa   29
#> 2          0.4     setosa    7
#> 3          0.3     setosa    7
#> 4          0.1     setosa    5
#> 5          0.5     setosa    1
#> 6          0.6     setosa    1
#> 7          1.4 versicolor    7
#> 8          1.5 versicolor   10
#> 9          1.3 versicolor   13
#> 10         1.6 versicolor    3
#> 11         1.0 versicolor    7
#> 12         1.1 versicolor    3
#> 13         1.8 versicolor    1
#> 14         1.2 versicolor    5
#> 15         1.7 versicolor    1
#> 16         2.5  virginica    3
#> 17         1.9  virginica    5
#> 18         2.1  virginica    6
#> 19         1.8  virginica   11
#> 20         2.2  virginica    3
#> 21         1.7  virginica    1
#> 22         2.0  virginica    6
#> 23         2.4  virginica    3
#> 24         2.3  virginica    8
#> 25         1.5  virginica    2
#> 26         1.6  virginica    1
#> 27         1.4  virginica    1
contract(iris,by=w~Species)
#>      Species Freq
#> 1     setosa   50
#> 2 versicolor   50
#> 3  virginica   50

library(MASS)
contract(housing,
         by=Sat~Infl+Type+Cont,
         weights=Freq)
#>      Infl      Type Cont Low Medium High
#> 1     Low     Tower  Low  21     21   28
#> 2  Medium     Tower  Low  34     22   36
#> 3    High     Tower  Low  10     11   36
#> 4     Low Apartment  Low  61     23   17
#> 5  Medium Apartment  Low  43     35   40
#> 6    High Apartment  Low  26     18   54
#> 7     Low    Atrium  Low  13      9   10
#> 8  Medium    Atrium  Low   8      8   12
#> 9    High    Atrium  Low   6      7    9
#> 10    Low   Terrace  Low  18      6    7
#> 11 Medium   Terrace  Low  15     13   13
#> 12   High   Terrace  Low   7      5   11
#> 13    Low     Tower High  14     19   37
#> 14 Medium     Tower High  17     23   40
#> 15   High     Tower High   3      5   23
#> 16    Low Apartment High  78     46   43
#> 17 Medium Apartment High  48     45   86
#> 18   High Apartment High  15     25   62
#> 19    Low    Atrium High  20     23   20
#> 20 Medium    Atrium High  10     22   24
#> 21   High    Atrium High   7     10   21
#> 22    Low   Terrace High  57     23   13
#> 23 Medium   Terrace High  31     21   13
#> 24   High   Terrace High   5      6   13

contract(housing,
         by=Sat~Infl+Type+Cont,
         weights=Freq,
         name="housing",force.name=TRUE
         )
#>      Infl      Type Cont housing.Low housing.Medium housing.High
#> 1     Low     Tower  Low          21             21           28
#> 2  Medium     Tower  Low          34             22           36
#> 3    High     Tower  Low          10             11           36
#> 4     Low Apartment  Low          61             23           17
#> 5  Medium Apartment  Low          43             35           40
#> 6    High Apartment  Low          26             18           54
#> 7     Low    Atrium  Low          13              9           10
#> 8  Medium    Atrium  Low           8              8           12
#> 9    High    Atrium  Low           6              7            9
#> 10    Low   Terrace  Low          18              6            7
#> 11 Medium   Terrace  Low          15             13           13
#> 12   High   Terrace  Low           7              5           11
#> 13    Low     Tower High          14             19           37
#> 14 Medium     Tower High          17             23           40
#> 15   High     Tower High           3              5           23
#> 16    Low Apartment High          78             46           43
#> 17 Medium Apartment High          48             45           86
#> 18   High Apartment High          15             25           62
#> 19    Low    Atrium High          20             23           20
#> 20 Medium    Atrium High          10             22           24
#> 21   High    Atrium High           7             10           21
#> 22    Low   Terrace High          57             23           13
#> 23 Medium   Terrace High          31             21           13
#> 24   High   Terrace High           5              6           13
```
