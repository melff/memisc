# Describe structure of Data Sets and Importers

The function `codeplan()` creates a data frame that describes the
structure of an item list (a `data.set` object or an `importer` object),
so that this structure can be stored and and recovered. The resulting
data frame has a particular print method that delimits the output to one
line per variable.

With `setCodeplan` an item list structure (as returned by `codeplan()`)
can be applied to a data frame or data set. It is also possible to use
an assignment like `codeplan(x) <- value` to a similar effect.

## Usage

``` r
codeplan(x)
# S4 method for class 'item.list'
codeplan(x)
# S4 method for class 'item'
codeplan(x)
setCodeplan(x,value)
# S4 method for class 'data.frame,codeplan'
setCodeplan(x,value)
# S4 method for class 'data.frame,NULL'
setCodeplan(x,value)
# S4 method for class 'data.set,codeplan'
setCodeplan(x,value)
# S4 method for class 'data.set,NULL'
setCodeplan(x,value)
# S4 method for class 'item,codeplan'
setCodeplan(x,value)
# S4 method for class 'item,NULL'
setCodeplan(x,value)
# S4 method for class 'atomic,codeplan'
setCodeplan(x,value)
# S4 method for class 'atomic,NULL'
setCodeplan(x,value)
codeplan(x) <- value
read_codeplan(filename,type)
write_codeplan(x,filename,type,pretty)
```

## Arguments

- x:

  for `codeplan(x)` an object that inherits from class `"item.list"`,
  i.e. can be a `"data.set"` object or an `"importer"` object, it can
  also be an object that inherits from class `"item"`. For
  `write_codeplan` an object from class `"codeplan"`.

- value:

  an object as it would be returned by `codeplan(x)` or `NULL`.

- filename:

  a character string, the name of the file that is to be read or to be
  written.

- type:

  a character string (either "yaml" or "json") oder NULL (the default),
  gives the type of the file into which the codeplan is written or from
  which it is read. If `type` is NULL then the file type is inferred
  from the file name ending (".yaml" or ",yml" for "yaml", ".json" for
  "json").

- pretty:

  a logical value, whether the JSON output created by
  `write_codeplan(...)` should be prettified.

## Value

If applicable, `codeplan` returns a list with additional S3 class
attribute `"codeplan"`. For arguments for which the relevant information
does not exist, the function returns `NULL`.

The list has at least one element or several elements, named after the
variable in the "item.list" or "data.set" `x`. Each list element is a
list itself with the following elements:

- `annotation`:

  a named character vector,

- `labels`:

  a named list of labels and labelled values

- `value.filter`:

  a list with at least two elements named "class" and "filter", and
  optionally another element named "range". The "class" element
  determines the class of the value filter and equals either
  "missing.values", "valid.values", or "valid.range". An element named
  "range" may only be needed if "class" is "missing.values", as it is
  possible (like in SPSS) to have *both* individual missing values and a
  range of missing values.

- `mode`:

  a character string that describes storage mode, such as `"character"`,
  `"integer"`, or `"numeric"`.

- `measurement`:

  a character string with the measurement level, `"nominal"`,
  `"ordinal"`, `"interval"`, or `"ratio"`.

If `codeplan(x)<-value` or `setCodeplan(x,value)` is used and `value` is
`NULL`, all the special information about annotation, labels, value
filters, etc. is removed from the resulting object, which then is
usually a mere atomic vector or data frame.

## Examples

``` r
Data1 <- data.set(
          vote = sample(c(1,2,3,8,9,97,99),size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),size=300,replace=TRUE),
          income = exp(rnorm(300,sd=.7))*2000
          )

Data1 <- within(Data1,{
  description(vote) <- "Vote intention"
  description(region) <- "Region of residence"
  description(income) <- "Household income"
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
cpData1 <- codeplan(Data1)

Data2 <- data.frame(
          vote = sample(c(1,2,3,8,9,97,99),size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),size=300,replace=TRUE),
          income = exp(rnorm(300,sd=.7))*2000
          )
codeplan(Data2) <- cpData1
codeplan(Data2)
#> 
#> vote:
#>   annotation:
#>     description: Vote intention
#>     Remark: This is not a real survey item, of course ...
#>   labels:
#>     Conservatives: 1.0
#>     Labour: 2.0
#>     Liberal Democrats: 3.0
#>     Don't know: 8.0
#>     Answer refused: 9.0
#>     Not applicable: 97.0
#>     Not asked in survey: 99.0
#>   value.filter:
#>     class: missing.values
#>     values:
#>     - 8.0
#>     - 9.0
#>     - 97.0
#>     - 99.0
#>   mode: numeric
#>   measurement: nominal
#> region:
#>   annotation:
#>     description: Region of residence
#>     Remark: This is not a real survey item, of course ...
#>   labels:
#>     England: 1.0
#>     Scotland: 2.0
#>     Wales: 3.0
#>     Not applicable: 97.0
#>     Not asked in survey: 99.0
#>   value.filter:
#>     class: missing.values
#>     values:
#>     - 97.0
#>     - 99.0
#>   mode: numeric
#>   measurement: nominal
#> income:
#>   annotation:
#>     description: Household income
#>     Remark: This is not a real survey item, of course ...
#>   mode: numeric
#>   measurement: ratio
#> 
codebook(Data2)
#> ================================================================================
#> 
#>    vote 'Vote intention'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 8, 9, 97, 99
#> 
#>    Values and labels              N Valid Total
#>                                                
#>     1   'Conservatives'          46  33.8  15.3
#>     2   'Labour'                 52  38.2  17.3
#>     3   'Liberal Democrats'      38  27.9  12.7
#>     8 M 'Don't know'             45        15.0
#>     9 M 'Answer refused'         44        14.7
#>    97 M 'Not applicable'         38        12.7
#>    99 M 'Not asked in survey'    37        12.3
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
#>     1   'England'               148  54.6  49.3
#>     2   'Scotland'               76  28.0  25.3
#>     3   'Wales'                  47  17.3  15.7
#>    99 M 'Not asked in survey'    29         9.7
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 
#> ================================================================================
#> 
#>    income 'Household income'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: ratio
#> 
#>         Min:   286.038
#>         Max: 10661.060
#>        Mean:  2650.676
#>    Std.Dev.:  1870.203
#> 
#>    Remark:
#>        This is not a real survey item, of course ...
#> 

# Note the difference between 'as.data.frame' and setting
# the codeplan to NULL:
Data2df <- as.data.frame(Data2)
codeplan(Data2) <- NULL
str(Data2)
#> 'data.frame':    300 obs. of  3 variables:
#>  $ vote  : num  97 1 1 8 1 3 9 3 8 9 ...
#>  $ region: num  2 3 2 1 2 99 3 3 1 99 ...
#>  $ income: num  3239 2499 543 1348 3543 ...
str(Data2df)
#> 'data.frame':    300 obs. of  3 variables:
#>  $ vote  : Factor w/ 3 levels "Conservatives",..: NA 1 1 NA 1 3 NA 3 NA NA ...
#>   ..- attr(*, "label")= chr "Vote intention"
#>  $ region: Factor w/ 3 levels "England","Scotland",..: 2 3 2 1 2 NA 3 3 1 NA ...
#>   ..- attr(*, "label")= chr "Region of residence"
#>  $ income: num  3239 2499 543 1348 3543 ...
#>   ..- attr(*, "label")= chr "Household income"
codeplan(Data2) <- NULL # Does not change anything

# Codeplans of survey items can also be inquired and manipulated:
vote <- Data1$vote
str(vote)
#>  Nmnl. item w/ 7 labels for 1,2,3,... + ms.v.  num [1:300] 97 8 1 97 3 97 9 2 2 3 ...
cp.vote <- codeplan(vote)
codeplan(vote) <- NULL
str(vote)
#>  num [1:300] 97 8 1 97 3 97 9 2 2 3 ...
codeplan(vote) <- cp.vote
vote
#> 
#> Item 'Vote intention' (measurement: nominal, type: double, length = 300) 
#> 
#>  [1:300] *Not applicable *Don't know Conservatives *Not applicable ...

fn.json <- paste0(tempfile(),".json")
write_codeplan(codeplan(Data1),filename=fn.json)
codeplan(Data2) <- read_codeplan(fn.json)
codeplan(Data2)
#> 
#> vote:
#>   annotation:
#>     description: Vote intention
#>     Remark: This is not a real survey item, of course ...
#>   labels:
#>     Conservatives: 1
#>     Labour: 2
#>     Liberal Democrats: 3
#>     Don't know: 8
#>     Answer refused: 9
#>     Not applicable: 97
#>     Not asked in survey: 99
#>   value.filter:
#>     class: missing.values
#>     values:
#>     - 8
#>     - 9
#>     - 97
#>     - 99
#>   mode: numeric
#>   measurement: nominal
#> region:
#>   annotation:
#>     description: Region of residence
#>     Remark: This is not a real survey item, of course ...
#>   labels:
#>     England: 1
#>     Scotland: 2
#>     Wales: 3
#>     Not applicable: 97
#>     Not asked in survey: 99
#>   value.filter:
#>     class: missing.values
#>     values:
#>     - 97
#>     - 99
#>   mode: numeric
#>   measurement: nominal
#> income:
#>   annotation:
#>     description: Household income
#>     Remark: This is not a real survey item, of course ...
#>   mode: numeric
#>   measurement: ratio
#> 
```
