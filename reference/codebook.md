# Generate a Codebook of a Data Set

Function `codebook` collects documentation about an item, or the items
in a data set or external data file. It returns an object that, when
`show`n, print this documentation in a nicely formatted way.

## Usage

``` r
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'item'
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'atomic'
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'factor'
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'data.set'
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'importer'
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'data.frame'
codebook(x, weights = NULL, unweighted = TRUE, ...)
# S4 method for class 'tbl_df'
codebook(x, weights = NULL, unweighted = TRUE, ...)
```

## Arguments

- x:

  an [`item`](https://melff.github.io/memisc/reference/items.md),
  numeric or character vector, factor,
  [`data.set`](https://melff.github.io/memisc/reference/dataSets.md),
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) or
  [`importer`](https://melff.github.io/memisc/reference/importers.md)
  object for `codebook()`

- weights:

  an optional vector of weights.

- unweighted:

  an optional logical vector; if weights are given, it determines of
  only summaries of weighted data are show or also summaries of
  unweighted data.

- ...:

  other arguments, currently ignored.

## Value

An object of class "codebook", for which a `show` method exists that
produces a nicely formatted output.

## Examples

``` r
Data <- data.set(
          vote = sample(c(1,2,3,8,9,97,99),size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),size=300,replace=TRUE),
          income = exp(rnorm(300,sd=.7))*2000
          )

Data <- within(Data,{
  description(vote) <- "Vote intention"
  description(region) <- "Region of residence"
  description(income) <- "Household income"
  wording(vote) <- "If a general election would take place next tuesday,
                    the candidate of which party would you vote for?"
  wording(income) <- "All things taken into account, how much do all
                    household members earn in sum?"
  foreach(x=c(vote,region),{
    measurement(x) <- "nominal"
    })
  measurement(income) <- "ratio"
  labels(vote) <- c(
                    Conservatives         =  1,
                    Labour                =  2,
                    "Liberal Democrats"   =  3,
                    "Don't know"          =  8,
                    "Answer refused"      =  9,
                    "Not applicable"      = 97,
                    "Not asked in survey" = 99)
  labels(region) <- c(
                    England               =  1,
                    Scotland              =  2,
                    Wales                 =  3,
                    "Not applicable"      = 97,
                    "Not asked in survey" = 99)
  foreach(x=c(vote,region,income),{
    annotation(x)["Remark"] <- "This is not a real survey item, of course ..."
    })
  missing.values(vote) <- c(8,9,97,99)
  missing.values(region) <- c(97,99)
})

description(Data)
#> 
#>  vote   'Vote intention'     
#>  region 'Region of residence'
#>  income 'Household income'   
#> 

codebook(Data)
#> ================================================================================
#> 
#>    vote 'Vote intention'
#> 
#>    "If a general election would take place next tuesday, the candidate of which
#>    party would you vote for?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 8, 9, 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'Conservatives'          33  28.0  11.0
#>     2   'Labour'                 41  34.7  13.7
#>     3   'Liberal Democrats'      44  37.3  14.7
#>     8 M 'Don't know'             40        13.3
#>     9 M 'Answer refused'         37        12.3
#>    97 M 'Not applicable'         53        17.7
#>    99 M 'Not asked in survey'    52        17.3
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
#> ================================================================================
#> 
#>    region 'Region of residence'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'England'               139  52.9  46.3
#>     2   'Scotland'               82  31.2  27.3
#>     3   'Wales'                  42  16.0  14.0
#>    99 M 'Not asked in survey'    37        12.3
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
#> ================================================================================
#> 
#>    income 'Household income'
#> 
#>    "All things taken into account, how much do all household members earn in
#>    sum?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: ratio
#> 
#>         Min:   289.338
#>         Max: 17629.525
#>        Mean:  2460.252
#>    Std.Dev.:  2073.515
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 

codebook(Data)$vote
#> ================================================================================
#> 
#>    vote 'Vote intention'
#> 
#>    "If a general election would take place next tuesday, the candidate of which
#>    party would you vote for?"
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 8, 9, 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'Conservatives'          33  28.0  11.0
#>     2   'Labour'                 41  34.7  13.7
#>     3   'Liberal Democrats'      44  37.3  14.7
#>     8 M 'Don't know'             40        13.3
#>     9 M 'Answer refused'         37        12.3
#>    97 M 'Not applicable'         53        17.7
#>    99 M 'Not asked in survey'    52        17.3
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
codebook(Data)[2]
#> ================================================================================
#> 
#>    region 'Region of residence'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'England'               139  52.9  46.3
#>     2   'Scotland'               82  31.2  27.3
#>     3   'Wales'                  42  16.0  14.0
#>    99 M 'Not asked in survey'    37        12.3
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 

codebook(Data[2])
#> ================================================================================
#> 
#>    region 'Region of residence'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'England'               139  52.9  46.3
#>     2   'Scotland'               82  31.2  27.3
#>     3   'Wales'                  42  16.0  14.0
#>    99 M 'Not asked in survey'    37        12.3
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
 
DataFr <- as.data.frame(Data)
DataHv <- as_haven(Data,user_na=TRUE)

codebook(DataFr)
#> ================================================================================
#> 
#>    vote 'Vote intention'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 3 levels
#> 
#>    Levels and labels          N Valid Total
#>                                            
#>     1 'Conservatives'        33  28.0  11.0
#>     2 'Labour'               41  34.7  13.7
#>     3 'Liberal Democrats'    44  37.3  14.7
#>    NA                       182        60.7
#> 
#> ================================================================================
#> 
#>    region 'Region of residence'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Factor with 3 levels
#> 
#>    Levels and labels     N Valid Total
#>                                       
#>     1 'England'        139  52.9  46.3
#>     2 'Scotland'        82  31.2  27.3
#>     3 'Wales'           42  16.0  14.0
#>    NA                   37        12.3
#> 
#> ================================================================================
#> 
#>    income 'Household income'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#> 
#>         Min:   289.3379
#>         Max: 17629.5251
#>        Mean:  2460.2518
#>    Std.Dev.:  2073.5155
#> 
codebook(DataHv)
#> ================================================================================
#> 
#>    vote 'Vote intention'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: undefined
#>    Missing values: 8, 9, 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'Conservatives'          33  28.0  11.0
#>     2   'Labour'                 41  34.7  13.7
#>     3   'Liberal Democrats'      44  37.3  14.7
#>     8 M 'Don't know'             40        13.3
#>     9 M 'Answer refused'         37        12.3
#>    97 M 'Not applicable'         53        17.7
#>    99 M 'Not asked in survey'    52        17.3
#> 
#> ================================================================================
#> 
#>    region 'Region of residence'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: integer
#>    Measurement: undefined
#>    Missing values: 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'England'               139  52.9  46.3
#>     2   'Scotland'               82  31.2  27.3
#>     3   'Wales'                  42  16.0  14.0
#>    99 M 'Not asked in survey'    37        12.3
#> 
#> ================================================================================
#> 
#>    income 'Household income'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#> 
#>         Min:   289.3379
#>         Max: 17629.5251
#>        Mean:  2460.2518
#>    Std.Dev.:  2073.5155
#> 

   
if (FALSE) { # \dontrun{
Write(description(Data),
           file="Data-desc.txt")
Write(codebook(Data),
           file="Data-cdbk.txt")
  } # }
```
