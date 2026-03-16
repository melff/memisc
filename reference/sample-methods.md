# Take a Sample from a Data Frame-like Object

The methods below are convenience short-cuts to take samples from data
frames and data sets. They result in a data frame or data set,
respectively, the rows of which are a sample of the complete data
frame/data set.

## Usage

``` r
# S4 method for class 'data.frame'
sample(x, size, replace = FALSE, prob = NULL)
# S4 method for class 'data.set'
sample(x, size, replace = FALSE, prob = NULL)
# S4 method for class 'importer'
sample(x, size, replace = FALSE, prob = NULL)
```

## Arguments

- x:

  a data frame or data set.

- size:

  an (optional) numerical value, the sample size, defaults to the total
  number of rows of `x`.

- replace:

  a logical value, determines whether sampling takes place with or
  without replacement.

- prob:

  a vector of sampling probabities or NULL.

## Value

A data frame or data set.

## Examples

``` r
for(.i in 1:4)
  print(sample(iris,5))
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 65           5.6         2.9          3.6         1.3 versicolor
#> 90           5.5         2.5          4.0         1.3 versicolor
#> 135          6.1         2.6          5.6         1.4  virginica
#> 94           5.0         2.3          3.3         1.0 versicolor
#> 23           4.6         3.6          1.0         0.2     setosa
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 41           5.0         3.5          1.3         0.3     setosa
#> 73           6.3         2.5          4.9         1.5 versicolor
#> 126          7.2         3.2          6.0         1.8  virginica
#> 25           4.8         3.4          1.9         0.2     setosa
#> 132          7.9         3.8          6.4         2.0  virginica
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 79           6.0         2.9          4.5         1.5 versicolor
#> 10           4.9         3.1          1.5         0.1     setosa
#> 71           5.9         3.2          4.8         1.8 versicolor
#> 116          6.4         3.2          5.3         2.3  virginica
#> 46           4.8         3.0          1.4         0.3     setosa
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#> 145          6.7         3.3          5.7         2.5 virginica
#> 34           5.5         4.2          1.4         0.2    setosa
#> 15           5.8         4.0          1.2         0.2    setosa
#> 150          5.9         3.0          5.1         1.8 virginica
#> 118          7.7         3.8          6.7         2.2 virginica
```
