# Assign a values to a variable for instances where a condition is met

The `%if%` operator allows to assign values to a variable only if a
condition is met i.e. results in `TRUE`. It is supposed to be used
similar to the `replace ... if` construct in Stata.

## Usage

``` r
expr %if% condition
# For example
# (variable <- value) %if% (other_variable == 0)
```

## Arguments

- expr:

  An expression that assigns a value to variable

- condition:

  A logical vector or a an expression that evaluates to a logical vector

## Details

The 'value' that is assigned to the variable in `expr` should either be
a scalar, a vector with as many elements as the condition vector has, or
as many elements as the number of elements in the condition vector that
are equal (or evaluate to) `TRUE`.

## Examples

``` r
(test_var <- 1) %if% (1:7 > 3)

test_var
#> [1] NA NA NA  1  1  1  1

(test_var <- 2) %if% (1:7 <= 3)
test_var
#> [1] 2 2 2 1 1 1 1

(test_var <- 100*test_var) %if% (1:7%%2==0)
test_var
#> [1]   2 200   2 100   1 100   1

# This creates a warning about non-matching lengths.
(test_var <- 500:501) %if% (1:7 <= 3)
#> Warning: Non-matching lengths in assignment
test_var
#> [1] 500 501 500 100   1 100   1

(test_var <- 501:503) %if% (1:7 <= 3)
test_var
#> [1] 501 502 503 100   1 100   1

(test_var <- 401:407) %if% (1:7 <= 3)
test_var
#> [1] 401 402 403 100   1 100   1
```
