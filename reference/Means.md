# Means for groups of observations

The function `Means()` creates a table of group means, optionally with
standard errors, confidence intervals, and numbers of valid
observations.

## Usage

``` r
Means(data, ...)
# S3 method for class 'data.frame'
Means(data,
    by, weights=NULL, subset=NULL,
    default=NA,
    se=FALSE, ci=FALSE, ci.level=.95,
    counts=FALSE, ...)
# S3 method for class 'formula'
Means(data, subset, weights, ...)
# S3 method for class 'numeric'
Means(data, ...)
# S3 method for class 'means.table'
as.data.frame(x, row.names=NULL, optional=TRUE, drop=TRUE, ...)
# S3 method for class 'xmeans.table'
as.data.frame(x, row.names=NULL, optional=TRUE, drop=TRUE, ...)
```

## Arguments

- data:

  an object usually containing data, or a formula.

  If `data` is a numeric vector or an object that can be coerced into a
  data frame, it is changed into a data frame and the data frame method
  of `Means()` is applied to it.

  If `data` is a formula, then a data frame is constructed from the
  variables in the formula and `Means` is applied to this data frame,
  while the formula is passed on as a `by=` argument.

- by:

  a formula, a vector of variable names or a data frame or list of
  factors.

  If `by` is a vector of variable names, they are extracted from `data`
  to define the groups for which means are computed, while the variables
  for which the means are computed are those not named in `by`.

  If `by` is a data frame or a list of factors, these are used to
  defined the groups for which means are computed, while the variables
  for which the means are computed are those not in `by`.

  If `by` is a formula, its left-hand side determines the variables of
  which means are computed, while its right-hand side determines the
  factors that define the groups.

- weights:

  an optional vector of weights, usually a variable in `data`.

- subset:

  an optional logical vector to select observations, usually the result
  of an expression in variables from `data`.

- default:

  a default value used for empty cells without observations.

- se:

  a logical value, indicates whether standard errors should be computed.

- ci:

  a logical value, indicates whether limits of confidence intervals
  should be computed.

- ci.level:

  a number, the confidence level of the confidence interval

- counts:

  a logical value, indicates whether numbers of valid observations
  should be reported.

- x:

  for [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), a
  result of `Means()`.

- row.names:

  an optional character vector. This argmument presently is
  inconsequential and only included for reasons of compatiblity with the
  standard methods of
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  an optional logical value. This argmument presently is inconsequential
  and only included for reasons of compatiblity with the standard
  methods of
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- drop:

  a logical value, determines whether "empty cells" should be dropped
  from the resulting data frame.

- ...:

  other arguments, either ignored or passed on to other methods where
  applicable.

## Value

An array that inherits classes "means.table" and "table". If `Means` was
called with `se=TRUE` or `ci=TRUE` then the result additionally inherits
class "xmeans.table".

## Examples

``` r
# Preparing example data
USstates <- as.data.frame(state.x77)
USstates <- within(USstates,{
    region <- state.region
    name <- state.name
    abb <- state.abb
    division <- state.division
})
USstates$w <- sample(runif(n=6),size=nrow(USstates),replace=TRUE)

# Using the data frame method
Means(USstates[c("Murder","division","region")],by=c("division","region"))
#> , ,  = Murder
#> 
#>                     region
#> division             Northeast     South North Central      West
#>   New England         3.383333                                  
#>   Middle Atlantic     7.400000                                  
#>   South Atlantic                9.775000                        
#>   East South Central           12.300000                        
#>   West South Central           10.475000                        
#>   East North Central                          7.780000          
#>   West North Central                          3.485714          
#>   Mountain                                              7.187500
#>   Pacific                                               7.260000
#> 
Means(USstates[c("Murder","division","region")],by=USstates[c("division","region")])
#> , ,  = Murder
#> 
#>                     region
#> division             Northeast     South North Central      West
#>   New England         3.383333                                  
#>   Middle Atlantic     7.400000                                  
#>   South Atlantic                9.775000                        
#>   East South Central           12.300000                        
#>   West South Central           10.475000                        
#>   East North Central                          7.780000          
#>   West North Central                          3.485714          
#>   Mountain                                              7.187500
#>   Pacific                                               7.260000
#> 
Means(USstates[c("Murder")],1)
#>         Mean
#> Murder 7.378
Means(USstates[c("Murder","region")],by=c("region"))
#>                
#> region             Murder
#>   Northeast      4.722222
#>   South         10.581250
#>   North Central  5.275000
#>   West           7.215385

# Using the formula method
# One 'dependent' variable
Means(Murder~1, data=USstates)
#>         Mean
#> Murder 7.378
Means(Murder~division, data=USstates)
#>                     
#> division                Murder
#>   New England         3.383333
#>   Middle Atlantic     7.400000
#>   South Atlantic      9.775000
#>   East South Central 12.300000
#>   West South Central 10.475000
#>   East North Central  7.780000
#>   West North Central  3.485714
#>   Mountain            7.187500
#>   Pacific             7.260000
Means(Murder~division, data=USstates,weights=w)
#>                     
#> division                Murder
#>   New England         3.334936
#>   Middle Atlantic     8.463128
#>   South Atlantic     10.412793
#>   East South Central 11.897595
#>   West South Central 10.889880
#>   East North Central  8.667188
#>   West North Central  3.677417
#>   Mountain            7.380093
#>   Pacific             6.525287
Means(Murder~division+region, data=USstates)
#> , ,  = Murder
#> 
#>                     region
#> division             Northeast     South North Central      West
#>   New England         3.383333                                  
#>   Middle Atlantic     7.400000                                  
#>   South Atlantic                9.775000                        
#>   East South Central           12.300000                        
#>   West South Central           10.475000                        
#>   East North Central                          7.780000          
#>   West North Central                          3.485714          
#>   Mountain                                              7.187500
#>   Pacific                                               7.260000
#> 
as.data.frame(Means(Murder~division+region, data=USstates))
#>              division        region    Murder
#> 1         New England     Northeast  3.383333
#> 2     Middle Atlantic     Northeast  7.400000
#> 12     South Atlantic         South  9.775000
#> 13 East South Central         South 12.300000
#> 14 West South Central         South 10.475000
#> 24 East North Central North Central  7.780000
#> 25 West North Central North Central  3.485714
#> 35           Mountain          West  7.187500
#> 36            Pacific          West  7.260000

# Standard errors and counts
Means(Murder~division, data=USstates, se=TRUE, counts=TRUE)
#> , , Statistic = Mean
#> 
#>                     Variable
#> division                 Murder
#>   New England         3.3833333
#>   Middle Atlantic     7.4000000
#>   South Atlantic      9.7750000
#>   East South Central 12.3000000
#>   West South Central 10.4750000
#>   East North Central  7.7800000
#>   West North Central  3.4857143
#>   Mountain            7.1875000
#>   Pacific             7.2600000
#> 
#> , , Statistic = SE
#> 
#>                     Variable
#> division                 Murder
#>   New England         0.4475241
#>   Middle Atlantic     1.7691806
#>   South Atlantic      0.9151015
#>   East South Central  1.0189864
#>   West South Central  1.5040916
#>   East North Central  1.4287757
#>   West North Central  1.0411597
#>   Mountain            0.8565791
#>   Pacific             1.4968634
#> 
#> , , Statistic = N
#> 
#>                     Variable
#> division                 Murder
#>   New England         6.0000000
#>   Middle Atlantic     3.0000000
#>   South Atlantic      8.0000000
#>   East South Central  4.0000000
#>   West South Central  4.0000000
#>   East North Central  5.0000000
#>   West North Central  7.0000000
#>   Mountain            8.0000000
#>   Pacific             5.0000000
#> 
drop(Means(Murder~division, data=USstates, se=TRUE, counts=TRUE))
#>                     Statistic
#> division                   Mean         SE          N
#>   New England         3.3833333  0.4475241  6.0000000
#>   Middle Atlantic     7.4000000  1.7691806  3.0000000
#>   South Atlantic      9.7750000  0.9151015  8.0000000
#>   East South Central 12.3000000  1.0189864  4.0000000
#>   West South Central 10.4750000  1.5040916  4.0000000
#>   East North Central  7.7800000  1.4287757  5.0000000
#>   West North Central  3.4857143  1.0411597  7.0000000
#>   Mountain            7.1875000  0.8565791  8.0000000
#>   Pacific             7.2600000  1.4968634  5.0000000
as.data.frame(Means(Murder~division, data=USstates, se=TRUE, counts=TRUE))
#>             division Variable      Mean        SE N
#> 1        New England   Murder  3.383333 0.4475241 6
#> 2    Middle Atlantic   Murder  7.400000 1.7691806 3
#> 3     South Atlantic   Murder  9.775000 0.9151015 8
#> 4 East South Central   Murder 12.300000 1.0189864 4
#> 5 West South Central   Murder 10.475000 1.5040916 4
#> 6 East North Central   Murder  7.780000 1.4287757 5
#> 7 West North Central   Murder  3.485714 1.0411597 7
#> 8           Mountain   Murder  7.187500 0.8565791 8
#> 9            Pacific   Murder  7.260000 1.4968634 5

# Confidence intervals
Means(Murder~division, data=USstates, ci=TRUE)
#> , , Statistic = Mean
#> 
#>                     Variable
#> division                 Murder
#>   New England         3.3833333
#>   Middle Atlantic     7.4000000
#>   South Atlantic      9.7750000
#>   East South Central 12.3000000
#>   West South Central 10.4750000
#>   East North Central  7.7800000
#>   West North Central  3.4857143
#>   Mountain            7.1875000
#>   Pacific             7.2600000
#> 
#> , , Statistic = Lower
#> 
#>                     Variable
#> division                 Murder
#>   New England         2.2329361
#>   Middle Atlantic    -0.2121697
#>   South Atlantic      7.6111289
#>   East South Central  9.0571304
#>   West South Central  5.6883091
#>   East North Central  3.8130827
#>   West North Central  0.9380882
#>   Mountain            5.1620124
#>   Pacific             3.1040410
#> 
#> , , Statistic = Upper
#> 
#>                     Variable
#> division                 Murder
#>   New England         4.5337305
#>   Middle Atlantic    15.0121697
#>   South Atlantic     11.9388711
#>   East South Central 15.5428696
#>   West South Central 15.2616909
#>   East North Central 11.7469173
#>   West North Central  6.0333404
#>   Mountain            9.2129876
#>   Pacific            11.4159590
#> 
drop(Means(Murder~division, data=USstates, ci=TRUE))
#>                     Statistic
#> division                   Mean      Lower      Upper
#>   New England         3.3833333  2.2329361  4.5337305
#>   Middle Atlantic     7.4000000 -0.2121697 15.0121697
#>   South Atlantic      9.7750000  7.6111289 11.9388711
#>   East South Central 12.3000000  9.0571304 15.5428696
#>   West South Central 10.4750000  5.6883091 15.2616909
#>   East North Central  7.7800000  3.8130827 11.7469173
#>   West North Central  3.4857143  0.9380882  6.0333404
#>   Mountain            7.1875000  5.1620124  9.2129876
#>   Pacific             7.2600000  3.1040410 11.4159590
as.data.frame(Means(Murder~division, data=USstates, ci=TRUE))
#>             division Variable      Mean      Lower     Upper
#> 1        New England   Murder  3.383333  2.2329361  4.533731
#> 2    Middle Atlantic   Murder  7.400000 -0.2121697 15.012170
#> 3     South Atlantic   Murder  9.775000  7.6111289 11.938871
#> 4 East South Central   Murder 12.300000  9.0571304 15.542870
#> 5 West South Central   Murder 10.475000  5.6883091 15.261691
#> 6 East North Central   Murder  7.780000  3.8130827 11.746917
#> 7 West North Central   Murder  3.485714  0.9380882  6.033340
#> 8           Mountain   Murder  7.187500  5.1620124  9.212988
#> 9            Pacific   Murder  7.260000  3.1040410 11.415959

# More than one dependent variable
Means(Murder+Illiteracy~division, data=USstates)
#>                     
#> division                 Murder Illiteracy
#>   New England         3.3833333  0.9166667
#>   Middle Atlantic     7.4000000  1.1666667
#>   South Atlantic      9.7750000  1.5000000
#>   East South Central 12.3000000  1.9500000
#>   West South Central 10.4750000  2.0000000
#>   East North Central  7.7800000  0.8000000
#>   West North Central  3.4857143  0.6285714
#>   Mountain            7.1875000  0.9500000
#>   Pacific             7.2600000  1.1400000
as.data.frame(Means(Murder+Illiteracy~division, data=USstates))
#>             division    Murder Illiteracy
#> 1        New England  3.383333  0.9166667
#> 2    Middle Atlantic  7.400000  1.1666667
#> 3     South Atlantic  9.775000  1.5000000
#> 4 East South Central 12.300000  1.9500000
#> 5 West South Central 10.475000  2.0000000
#> 6 East North Central  7.780000  0.8000000
#> 7 West North Central  3.485714  0.6285714
#> 8           Mountain  7.187500  0.9500000
#> 9            Pacific  7.260000  1.1400000

# Confidence intervals
Means(Murder+Illiteracy~division, data=USstates, ci=TRUE)
#> , , Statistic = Mean
#> 
#>                     Variable
#> division                 Murder Illiteracy
#>   New England         3.3833333  0.9166667
#>   Middle Atlantic     7.4000000  1.1666667
#>   South Atlantic      9.7750000  1.5000000
#>   East South Central 12.3000000  1.9500000
#>   West South Central 10.4750000  2.0000000
#>   East North Central  7.7800000  0.8000000
#>   West North Central  3.4857143  0.6285714
#>   Mountain            7.1875000  0.9500000
#>   Pacific             7.2600000  1.1400000
#> 
#> , , Statistic = Lower
#> 
#>                     Variable
#> division                 Murder Illiteracy
#>   New England         2.2329361  0.6167655
#>   Middle Atlantic    -0.2121697  0.6495522
#>   South Atlantic      7.6111289  1.0807969
#>   East South Central  9.0571304  1.3617494
#>   West South Central  5.6883091  0.8748353
#>   East North Central  3.8130827  0.6758336
#>   West North Central  0.9380882  0.5126359
#>   Mountain            5.1620124  0.3990592
#>   Pacific             3.1040410  0.4343240
#> 
#> , , Statistic = Upper
#> 
#>                     Variable
#> division                 Murder Illiteracy
#>   New England         4.5337305  1.2165679
#>   Middle Atlantic    15.0121697  1.6837812
#>   South Atlantic     11.9388711  1.9192031
#>   East South Central 15.5428696  2.5382506
#>   West South Central 15.2616909  3.1251647
#>   East North Central 11.7469173  0.9241664
#>   West North Central  6.0333404  0.7445070
#>   Mountain            9.2129876  1.5009408
#>   Pacific            11.4159590  1.8456760
#> 
as.data.frame(Means(Murder+Illiteracy~division, data=USstates, ci=TRUE))
#>              division   Variable       Mean      Lower      Upper
#> 1         New England     Murder  3.3833333  2.2329361  4.5337305
#> 2     Middle Atlantic     Murder  7.4000000 -0.2121697 15.0121697
#> 3      South Atlantic     Murder  9.7750000  7.6111289 11.9388711
#> 4  East South Central     Murder 12.3000000  9.0571304 15.5428696
#> 5  West South Central     Murder 10.4750000  5.6883091 15.2616909
#> 6  East North Central     Murder  7.7800000  3.8130827 11.7469173
#> 7  West North Central     Murder  3.4857143  0.9380882  6.0333404
#> 8            Mountain     Murder  7.1875000  5.1620124  9.2129876
#> 9             Pacific     Murder  7.2600000  3.1040410 11.4159590
#> 10        New England Illiteracy  0.9166667  0.6167655  1.2165679
#> 11    Middle Atlantic Illiteracy  1.1666667  0.6495522  1.6837812
#> 12     South Atlantic Illiteracy  1.5000000  1.0807969  1.9192031
#> 13 East South Central Illiteracy  1.9500000  1.3617494  2.5382506
#> 14 West South Central Illiteracy  2.0000000  0.8748353  3.1251647
#> 15 East North Central Illiteracy  0.8000000  0.6758336  0.9241664
#> 16 West North Central Illiteracy  0.6285714  0.5126359  0.7445070
#> 17           Mountain Illiteracy  0.9500000  0.3990592  1.5009408
#> 18            Pacific Illiteracy  1.1400000  0.4343240  1.8456760

# Some 'non-standard' but still valid usages:
with(USstates,
     Means(Murder~division+region,subset=region!="Northeast"))
#> , ,  = Murder
#> 
#>                     region
#> division                 South North Central      West
#>   South Atlantic      9.775000                        
#>   East South Central 12.300000                        
#>   West South Central 10.475000                        
#>   East North Central                7.780000          
#>   West North Central                3.485714          
#>   Mountain                                    7.187500
#>   Pacific                                     7.260000
#> 

with(USstates,
     Means(Murder,by=list(division,region)))
#> , , Murder
#> 
#>                    Northeast     South North Central      West
#> New England         3.383333                                  
#> Middle Atlantic     7.400000                                  
#> South Atlantic                9.775000                        
#> East South Central           12.300000                        
#> West South Central           10.475000                        
#> East North Central                          7.780000          
#> West North Central                          3.485714          
#> Mountain                                              7.187500
#> Pacific                                               7.260000
#> 
```
