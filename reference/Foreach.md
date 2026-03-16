# Loop over Variables in a Data Frame or Environment

`foreach` evaluates an expression given as untagged argument by
substituting in variables. The expression may also contain assignments,
which take effect in the caller's environment.

## Usage

``` r
foreach(...,.sorted,.outer=FALSE)
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

  If this argument missing, its default value is TRUE, if `foreach()` is
  called in the global environment, otherwise it is FALSE.

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
     foreach(x=a:c,
             print(description(x))
            ))
#> [1] "First item in questionnaire"
#> [1] "Third item in questionnaire"

# Since .sorted=TRUE, the colon operator creates a range 
# of alphabetically sorted variables.
with(ds[c(3,1,2)],
     foreach(x=a:c,
             print(description(x)),
             .sorted=TRUE
            ))
#> [1] "First item in questionnaire"
#> [1] "Second item in questionnaire"
#> [1] "Third item in questionnaire"

# The variables in reverse order
with(ds,
     foreach(x=c:a,
             print(description(x))
            ))
#> [1] "Third item in questionnaire"
#> [1] "Second item in questionnaire"
#> [1] "First item in questionnaire"

# The colon operator can be combined with the 
# concatenation function
with(ds,
     foreach(x=c(a:b,c,c,b:a),
             print(description(x))
            ))
#> [1] "First item in questionnaire"
#> [1] "Second item in questionnaire"
#> [1] "Third item in questionnaire"
#> [1] "Third item in questionnaire"
#> [1] "Second item in questionnaire"
#> [1] "First item in questionnaire"

# Variables can also be selected by regular expressions.
with(ds,
     foreach(x=rx("[a-b]"),
             print(description(x))
            ))
#> [1] "First item in questionnaire"
#> [1] "Second item in questionnaire"

# A demonstration for '.outer=TRUE'
foreach(l=letters[1:2],
        i=1:3,
        cat(paste0(l,i,"\n")),
        .outer=TRUE)
#> a1
#> b1
#> a2
#> b2
#> a3
#> b3
```
