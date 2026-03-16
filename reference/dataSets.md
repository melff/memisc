# Data Set Objects

`"data.set"` objects are collections of `"item"` objects, with similar
semantics as data frames. They are distinguished from data frames so
that coercion by `as.data.fame` leads to a data frame that contains only
vectors and factors. Nevertheless most methods for data frames are
inherited by data sets, except for the method for the `within` generic
function. For the `within` method for data sets, see the details
section.

Thus data preparation using data sets retains all informations about
item annotations, labels, missing values etc. While (mostly automatic)
conversion of data sets into data frames makes the data amenable for the
use of R's statistical functions.

`dsView` is a function that displays data sets in a similar manner as
`View` displays data frames. (`View` works with data sets as well, but
changes them first into data frames.)

## Usage

``` r
data.set(...,row.names = NULL, check.rows = FALSE, check.names = TRUE,
    stringsAsFactors = FALSE, document = NULL)
as.data.set(x, row.names=NULL, ...)
# S4 method for class 'list'
as.data.set(x,row.names=NULL,...)
is.data.set(x)
# S3 method for class 'data.set'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
# S4 method for class 'data.set'
within(data, expr, ...)

dsView(x)

# S4 method for class 'data.set'
head(x,n=20,...)
# S4 method for class 'data.set'
tail(x,n=20,...)
```

## Arguments

- ...:

  For the `data.set` function several vectors or items, for `within`
  further, ignored arguments.

- row.names, check.rows, check.names, stringsAsFactors, optional:

  arguments as in [`data.frame`](https://rdrr.io/r/base/data.frame.html)
  or [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html),
  respectively.

- document:

  NULL or an optional character vector that contains documenation of the
  data.

- x:

  for `is.data.set(x)`, any object; for `as.data.frame(x,...)` and
  `dsView(x)` a "data.set" object.

- data:

  a data set, that is, an object of class "data.set".

- expr:

  an expression, or several expressions enclosed in curly braces.

- n:

  integer; the number of rows to be shown by `head` or `tail`

## Details

The `as.data.frame` method for data sets is just a copy of the method
for list. Consequently, all items in the data set are coerced in
accordance to their
[`measurement`](https://melff.github.io/memisc/reference/measurement.md)
setting, see
[`as.vector,item-method`](https://melff.github.io/memisc/reference/items-to-vectors.md)
and
[`measurement`](https://melff.github.io/memisc/reference/measurement.md).

The `within` method for data sets has the same effect as the
[`within`](https://rdrr.io/r/base/with.html) method for data frames,
apart from two differences: all results of the computations are coerced
into items if they have the appropriate length, otherwise, they are
automatically dropped.

Currently only one method for the generic function `as.data.set` is
defined: a method for "importer" objects.

## Value

`data.set` and the `within` method for data sets returns a "data.set"
object, `is.data.set` returns a logical value, and `as.data.frame`
returns a data frame.

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

  # These to variables do not appear in the
  # the resulting data set, since they have the wrong length.
  junk1 <- 1:5
  junk2 <- matrix(5,4,4)
  
})
#> Warning: Variables 'junk1','junk2' have wrong length, removing them.
# Since data sets may be huge, only a
# part of them are 'show'n
Data
#> 
#> Data set with 300 observations and 3 variables
#> 
#>                    vote               region    income
#>  1      *Not applicable *Not asked in survey 3384.5617
#>  2 *Not asked in survey             Scotland 2228.3955
#>  3        Conservatives              England 1597.7063
#>  4          *Don't know              England 5972.8596
#>  5               Labour              England  670.5351
#>  6               Labour              England  988.8037
#>  7        Conservatives *Not asked in survey 1758.9525
#>  8    Liberal Democrats *Not asked in survey 1291.6376
#>  9          *Don't know *Not asked in survey 2876.3122
#> 10    Liberal Democrats              England 2738.8012
#> 11               Labour *Not asked in survey 1349.7776
#> 12        Conservatives              England 2850.4797
#> 13 *Not asked in survey              England 1439.1641
#> 14    Liberal Democrats              England 4178.9150
#> 15               Labour *Not asked in survey 1362.5640
#> 16    Liberal Democrats                Wales  561.1009
#> 17      *Not applicable                Wales  975.9246
#> 18 *Not asked in survey              England  697.2254
#> 19               Labour             Scotland 4468.6364
#> 20        Conservatives *Not asked in survey 1696.1056
#> 21               Labour              England 3972.9354
#> 22          *Don't know              England 1241.3736
#> 23        Conservatives             Scotland 1374.9910
#> 24      *Not applicable              England 2958.4366
#> 25          *Don't know             Scotland 6693.7717
#> .. .................... .................... .........
#> (25 of 300 observations shown)

if (FALSE) { # \dontrun{

# If we insist on seeing all, we can use 'print' instead
print(Data)
} # }

str(Data)
#> Data set with 300 obs. of 3 variables:
#>  $ vote  : Nmnl. item w/ 7 labels for 1,2,3,... + ms.v.  num  97 99 1 8 2 2 1 3 8 3 ...
#>  $ region: Nmnl. item w/ 5 labels for 1,2,3,... + ms.v.  num  99 2 1 1 1 1 99 99 99 1 ...
#>  $ income: Rto. item  num  3385 2228 1598 5973 671 ...
summary(Data)
#>                    vote                     region        income       
#>  Conservatives       :38   England             :141   Min.   :  307.6  
#>  Labour              :49   Scotland            : 86   1st Qu.: 1348.5  
#>  Liberal Democrats   :41   Wales               : 39   Median : 1914.5  
#>  *Don't know         :43   *Not asked in survey: 34   Mean   : 2512.3  
#>  *Answer refused     :44                              3rd Qu.: 3046.3  
#>  *Not applicable     :44                              Max.   :16272.5  
#>  *Not asked in survey:41                                               

if (FALSE) { # \dontrun{
# If we want to 'View' a data set we can use 'dsView'
dsView(Data)
# Works also, but changes the data set into a data frame first:
View(Data)
} # }

Data[[1]]
#> 
#> Item 'Vote intention' (measurement: nominal, type: double, length = 300) 
#> 
#>  [1:300] *Not applicable *Not asked in survey Conservatives *Don't know ...
Data[1,]
#> 
#> Data set with 1 observations and 3 variables
#> 
#>              vote               region   income
#> 1 *Not applicable *Not asked in survey 3384.562
head(as.data.frame(Data))
#>            vote   region    income
#> 1          <NA>     <NA> 3384.5617
#> 2          <NA> Scotland 2228.3955
#> 3 Conservatives  England 1597.7063
#> 4          <NA>  England 5972.8596
#> 5        Labour  England  670.5351
#> 6        Labour  England  988.8037

EnglandData <- subset(Data,region == "England")
EnglandData
#> 
#> Data set with 141 observations and 3 variables
#> 
#>                    vote  region    income
#>  1        Conservatives England 1597.7063
#>  2          *Don't know England 5972.8596
#>  3               Labour England  670.5351
#>  4               Labour England  988.8037
#>  5    Liberal Democrats England 2738.8012
#>  6        Conservatives England 2850.4797
#>  7 *Not asked in survey England 1439.1641
#>  8    Liberal Democrats England 4178.9150
#>  9 *Not asked in survey England  697.2254
#> 10               Labour England 3972.9354
#> 11          *Don't know England 1241.3736
#> 12      *Not applicable England 2958.4366
#> 13               Labour England 1137.4416
#> 14        Conservatives England 1144.7375
#> 15          *Don't know England 1237.6942
#> 16               Labour England 1706.2591
#> 17        Conservatives England 2678.4200
#> 18    Liberal Democrats England 3770.3668
#> 19          *Don't know England 1146.1415
#> 20    Liberal Democrats England 1071.7657
#> 21    Liberal Democrats England 1049.9579
#> 22      *Answer refused England  829.9630
#> 23    Liberal Democrats England 2676.8390
#> 24               Labour England 1802.0925
#> 25 *Not asked in survey England 4175.8977
#> .. .................... ....... .........
#> (25 of 141 observations shown)

xtabs(~vote+region,data=Data)
#>                    region
#> vote                England Scotland Wales
#>   Conservatives          16       13     4
#>   Labour                 23       16     4
#>   Liberal Democrats      20       12     4
xtabs(~vote+region,data=within(Data, vote <- include.missings(vote)))
#>                       region
#> vote                   England Scotland Wales
#>   Conservatives             16       13     4
#>   Labour                    23       16     4
#>   Liberal Democrats         20       12     4
#>   *Don't know               21       12     6
#>   *Answer refused           19       14     7
#>   *Not applicable           24       10     5
#>   *Not asked in survey      18        9     9
```
