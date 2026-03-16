# Apply a function to ranges of variables

`xapply` evaluates an expression given as second argument by
substituting in variables. The results are collected in a list or array
in a similar way as done by `Sapply` or `lapply`.

## Usage

``` r
xapply(...,.sorted,simplify=TRUE,USE.NAMES=TRUE,.outer=FALSE)
```

## Arguments

- ...:

  tagged and untagged arguments. The tagged arguments define the
  'variables' that are looped over, the first untagged argument defines
  the expression wich is evaluated.

- .sorted:

  an optional logical value; relevant only when a range of variable is
  specified using the column operator "`:`". Decises whether variable
  names should be sorted alphabetically before the range of variables
  are created.

  If this argument missing, its default value is TRUE, if `xapply()` is
  called in the global environment, otherwise it is FALSE.

- simplify:

  a logical value; should the result be simplifies in `Sapply`?

- USE.NAMES:

  a logical value or a positive integer. If an integer, determines which
  variable is used to name the highest dimension of the result (its
  columns, in case is it a matrix). If TRUE, the first variable is used.

- .outer:

  an optional logical value; if TRUE, each combination of the variables
  is used to evaluate the expression, if FALSE (the default) then the
  variables all need to have the same length and the corresponding
  values of the variables are used in the evaluation of the expression.

## Examples

``` r
x <- 1:3
y <- -(1:3)
z <- c("Uri","Schwyz","Unterwalden")
print(x)
#> [1] 1 2 3
print(y)
#> [1] -1 -2 -3
print(z)
#> [1] "Uri"         "Schwyz"      "Unterwalden"
foreach(var=c(x,y,z),          # assigns names
  names(var) <- letters[1:3]   # to the elements of x, y, and z
  )
print(x)
#> a b c 
#> 1 2 3 
print(y)
#>  a  b  c 
#> -1 -2 -3 
print(z)
#>             a             b             c 
#>         "Uri"      "Schwyz" "Unterwalden" 

ds <- data.set(
        a = c(1,2,3,2,3,8,9),
        b = c(2,8,3,2,1,8,9),
        c = c(1,3,2,1,2,8,8)
      )
print(ds)
#>   a b c
#> 1 1 2 1
#> 2 2 8 3
#> 3 3 3 2
#> 4 2 2 1
#> 5 3 1 2
#> 6 8 8 8
#> 7 9 9 8
ds <- within(ds,{ 
      description(a) <- "First item in questionnaire"
      description(b) <- "Second item in questionnaire"
      description(c) <- "Third item in questionnaire"
      
      wording(a) <- "What number do you like first?"
      wording(b) <- "What number do you like second?"
      wording(c) <- "What number do you like third?"

      foreach(x=a:c,{ # Lazy data documentation:
        labels(x) <- c(    # a,b,c get value labels in one statement
                         one = 1,
                         two = 2,
                       three = 3,
                "don't know" = 8,
         "refused to answer" = 9)
        missing.values(x) <- c(8,9)
        })
      })
      
codebook(ds)
#> ================================================================================
#> 
#>    a 'First item in questionnaire'
#> 
#>    "What number do you like first?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: interval
#>    Missing values: 8, 9
#> 
#>    Values and labels           N Valid Total
#>                                             
#>    1   'one'                   1  20.0  14.3
#>    2   'two'                   2  40.0  28.6
#>    3   'three'                 2  40.0  28.6
#>    8 M 'don't know'            1        14.3
#>    9 M 'refused to answer'     1        14.3
#>                                             
#>         Min: 1.000                          
#>         Max: 3.000                          
#>        Mean: 2.200                          
#>    Std.Dev.: 0.748                          
#> 
#> ================================================================================
#> 
#>    b 'Second item in questionnaire'
#> 
#>    "What number do you like second?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: interval
#>    Missing values: 8, 9
#> 
#>    Values and labels           N Valid Total
#>                                             
#>    1   'one'                   1  25.0  14.3
#>    2   'two'                   2  50.0  28.6
#>    3   'three'                 1  25.0  14.3
#>    8 M 'don't know'            2        28.6
#>    9 M 'refused to answer'     1        14.3
#>                                             
#>         Min: 1.000                          
#>         Max: 3.000                          
#>        Mean: 2.000                          
#>    Std.Dev.: 0.707                          
#> 
#> ================================================================================
#> 
#>    c 'Third item in questionnaire'
#> 
#>    "What number do you like third?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: interval
#>    Missing values: 8, 9
#> 
#>    Values and labels     N Valid Total
#>                                       
#>    1   'one'             2  40.0  28.6
#>    2   'two'             2  40.0  28.6
#>    3   'three'           1  20.0  14.3
#>    8 M 'don't know'      2        28.6
#>                                       
#>         Min: 1.000                    
#>         Max: 3.000                    
#>        Mean: 1.800                    
#>    Std.Dev.: 0.748                    
#> 

# The colon-operator respects the order of the variables
# in the data set, if .sorted=FALSE
with(ds[c(3,1,2)],
     xapply(x=a:c,
            description(x)
            ))
#>                             a                             c 
#> "First item in questionnaire" "Third item in questionnaire" 

# Since .sorted=TRUE, the colon operator creates a range 
# of alphabetically sorted variables.
with(ds[c(3,1,2)],
     xapply(x=a:c,
            description(x),
            .sorted=TRUE
            ))
#>                              a                              b 
#>  "First item in questionnaire" "Second item in questionnaire" 
#>                              c 
#>  "Third item in questionnaire" 

# The variables in reverse order
with(ds,
     xapply(x=c:a,
             description(x)
            ))
#>                              c                              b 
#>  "Third item in questionnaire" "Second item in questionnaire" 
#>                              a 
#>  "First item in questionnaire" 

# The colon operator can be combined with the 
# concatenation function
with(ds,
     xapply(x=c(a:b,c,c,b:a),
             description(x)
            ))
#>                              a                              b 
#>  "First item in questionnaire" "Second item in questionnaire" 
#>                              c                              c 
#>  "Third item in questionnaire"  "Third item in questionnaire" 
#>                              b                              a 
#> "Second item in questionnaire"  "First item in questionnaire" 

# Variables can also be selected by regular expressions.
with(ds,
     xapply(x=rx("[a-b]"),
             description(x)
            ))
#>                              a                              b 
#>  "First item in questionnaire" "Second item in questionnaire" 

# Demonstrating the effects of the 'USE.NAMES' argument.
with(ds,
     xapply(x=a:c,mean(x)))
#>        a        b        c 
#> 4.000000 4.714286 3.571429 

with(ds,
     xapply(x=a:c,mean(x),
     USE.NAMES=FALSE))
#> [1] 4.000000 4.714286 3.571429

t(with(ds,
      xapply(i=1:3,
             x=a:c,
             c(Index=i,
               Mean=mean(x)),
      USE.NAMES=2)))
#>   Index     Mean
#> a     1 4.000000
#> b     2 4.714286
#> c     3 3.571429

# Result with 'simplify=FALSE'
with(ds,
     xapply(x=a:c,mean(x),
     simplify=FALSE))
#> $a
#> [1] 4
#> 
#> $b
#> [1] 4.714286
#> 
#> $c
#> [1] 3.571429
#> 

# It is also possible to loop over functions:
xapply(fun=c(exp,log),
       fun(1))
#>      exp      log 
#> 2.718282 0.000000 

# Two demonstrations for '.outer=TRUE'
with(ds,
      xapply(x=a:c,
             y=a:c,
             cov(x,y),
             .outer=TRUE))
#>    y
#> x           a         b        c
#>   a 10.000000  7.833333 9.500000
#>   b  7.833333 11.904762 9.023810
#>   c  9.500000  9.023810 9.619048

with(ds,
      xapply(x=a:c,
             y=a:c,
             fun=c(cov,cor),
             fun(x,y),
             .outer=TRUE))
#> , , fun = cov
#> 
#>    y
#> x           a         b        c
#>   a 10.000000  7.833333 9.500000
#>   b  7.833333 11.904762 9.023810
#>   c  9.500000  9.023810 9.619048
#> 
#> , , fun = cor
#> 
#>    y
#> x           a         b         c
#>   a 1.0000000 0.7179369 0.9686292
#>   b 0.7179369 1.0000000 0.8432639
#>   c 0.9686292 0.8432639 1.0000000
#> 
```
