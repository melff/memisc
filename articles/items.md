# Handling questionnaire items

## Motivation

R is a great tool to do data analysis and for data management tasks that
arise in the context of big data analytics. Nevertheless there is still
room for improvement in terms of the support for data management tasks
that arise in the social sciences, especially when it comes to handling
data that come from social surveys and opinion surveys. The main reason
for this is that the way that questionnaire item responses as they are
usually coded in machine-readable survey data sets do not directly and
easily translate into R’s data types for numeric and categorical data,
that is, numerical vectors and factors. As a consequence, many social
scientists exercise their everyday data management tasks with commercial
software packages such as SPSS or Stata, but there may be social
scientists who either cannot afford such commercial software or prefer
to use, out of principle, open-source software for all steps of data
management and analysis.

It is one of the aim of the “memisc” package to provide a bridge between
social science data sets of variables that contain coded responses to
questionnaire items, with their typical structures involving labelled
numeric response codes and numeric codes declared as “missing values”.
As an illustrative example, suppose in a pre-election survey,
respondents are asked about which party they are going to vote for in
their constituency in the framework of a first-past-the-post electoral
system. Suppose the response categories offered to the respondents are
“Conservative”, “Labour”, “Liberal Democrat”, “Other party”.[¹](#fn1) A
survey agency that actually conducts the interviews with a sample of
voters may, according to common practice, use the following codes to
collect the responses to the question about the vote intention:

| Response category | Code |       |
|:------------------|-----:|-------|
| Conservative      |    1 |       |
| Labour            |    2 |       |
| Liberal Democrat  |    3 |       |
| Other Party       |    4 |       |
| Will not vote     |    9 |       |
| Don’t know        |   97 | \(M\) |
| Answer refused    |   98 | \(M\) |
| Not applicable    |   99 | \(M\) |

In data sets that contain the results of such coding are essentially
numeric data – with some additional information about the “value labels”
(the labels attached to the numeric values) and about the “missing
values” (those numeric values that indicate responses that one usually
does not want to include into statistical analysis). While this coding
frame for responses to survey questionnaires is far from uncommon in the
social sciences, it is not straightforward to retain this information in
*R* objects. Here there are two main alternatives, (1) one could store
the responses as a numeric vector, thereby losing the information about
the labelled values, or (2) one could store the responses as a factor,
thereby losing the information contained in the codes. Either way, one
will lose the information about the “missing values”. Of course, one can
filter out these missing values before data analysis by replacing them
with `NA`, but it would convenient to have facilities that do that
automatically.

## Standard attributes of survey items

The “memisc” package introduces a new data type (more correctly an `S4`
class) that allows to handle such data, that allows to adjust labels or
missing values definitions and to translate such data as needed either
into numeric vectors of factors, thereby automatically filtering out the
missing values. This data time (or `S4` class) is, for lack of a better
term, called `"item"`. In general, users do not bother with the
construction of such item vectors. Usually they are generated when data
sets are imported from data files in SPSS or Stata format. This page is
mainly concerned with describing the structure of such item vectors and
how they can be manipulated in the data management step that usually
precedes data analysis. It is thus possible to do all the data
management in *R* from importing the pristine data obtained from data
archives or other data providers, such as the survey institutes to which
a principal investigator has delegated data collection. Of course, the
facilities introduced by the `"item"` data type also allow to create
appropriate representations of survey item responses if a principal
investigator obtains only raw numeric codes. In the following, the
construction of `"item"` vectors from raw numeric data is mainly used to
highlight their structure.

### Value labels

    Loading required package: lattice

    Loading required package: MASS

    Attaching package: 'memisc'

    The following objects are masked from 'package:stats':

        contr.sum, contr.treatment, contrasts

    The following object is masked from 'package:base':

        as.array

Suppose a numeric vector of responses to the question about their vote
intention coded using the coding frame shown above looks as follows

``` r
voteint
```

      [1]  4  3  9  2 97 99  9  9  1  1  3  3  9  3  9  1  1  9  9  3  1  9  1  9  9
     [26]  9 98 99  9  2  1  1  4  9  1  1  1 98  2  9  2  9  1  1  3  1  2  3  1  2
     [51]  9  1  9 97  9  1  9  1  9  9  1  9 97  9 97  9  4  2  9  2  9  1  9  2  4
     [76]  1  2  1  2  9  9  4  9 97  3  1  1  1  9  9  1  9  3 99  3  4  4  3  1  9
    [101]  4 97  1 99  2  2 98  3  3 98  1  9 98 99  1  3  9  9  2  1  1  9  1  2  1
    [126]  9  9  1  4  9  9  1  4  4  9 99  3  9  9  9  3  4  9  9  4  4  9  4  4  9
    [151]  2  1  1  1  1  9  9  9  1  3  1  2 99  3  2  9  2 99  2  3  9  1  1  1  2
    [176]  9  4  1 98  3 99 99  9  9  3  9  1  2  1  9  2  4 98  1  4 99  9  2  2  2

This numeric vector is transformed into an `"item"` vector by attaching
labels to the codes. The *R* code to attach labels that reflect the
coding frame shown above may look like follows (if formatted nicely):

``` r
# This is to be run *after* memisc has been loaded.
labels(voteint) <- c(Conservative       =  1,
                     Labour             =  2,
                     "Liberal Democrat" =  3, # We have whitespace in the label, 
                     "Other Party"      =  4, # so we need quotation marks
                     "Will not vote"    =  9,
                     "Don't know"       = 97,
                     "Answer refused"   = 98,
                     "Not applicable"   = 99)
```

`voteint` is now an item vector, for which a particular `"show"` method
is defined:

``` r
class(voteint)
```

    [1] "double.item"
    attr(,"package")
    [1] "memisc"

``` r
str(voteint)
```

     Nmnl. item w/ 8 labels for 1,2,3,...  num [1:200] 4 3 9 2 97 99 9 9 1 1 ...

``` r
voteint
```

    Item (measurement: nominal, type: double, length = 200) 

     [1:200] Other Party Liberal Democrat Will not vote Labour Don't know ...

Like with factors, if *R* shows the contents of the vector, the labels
are shown (instead of the codes). Since item vectors typically are quite
long, because they come from interviewing a survey sample and usual
survey sample sizes are about 2000, we usually do not want to see all
the values in the vector. `"memisc"` anticipates this and shows at most
a single line of output. (In the output, also the “level of measurement”
is shown, which at this point does not have a consquence. It will become
clear later what the implications of the “level of measurement” are.)

In line with the usual semantics `labels(voteint)` will now show us a
description of the labels and to which values they are assigned:

``` r
labels(voteint)
```

     Values and labels:

        1 'Conservative'    
        2 'Labour'          
        3 'Liberal Democrat'
        4 'Other Party'     
        9 'Will not vote'   
       97 'Don't know'      
       98 'Answer refused'  
       99 'Not applicable'  

Now if we rather want shorter labels, we can change them either by
something like `labels(voteint) <- ...` or by changing the labels using
[`relabel()`](https://melff.github.io/memisc/reference/relabel.md):

``` r
voteint <- relabel(voteint,
                   "Conservative"     = "Cons",
                   "Labour"           = "Lab",
                   "Liberal Democrat" = "LibDem",
                   "Other Party"      = "Other",
                   "Will not vote"    = "NoVote",
                   "Don't know"       = "DK",
                   "Answer refused"   = "Refused",
                   "Not applicable"   = "N.a.")
```

Let us take a look at the result:

``` r
labels(voteint)
```

     Values and labels:

        1 'Cons'   
        2 'Lab'    
        3 'LibDem' 
        4 'Other'  
        9 'NoVote' 
       97 'DK'     
       98 'Refused'
       99 'N.a.'   

``` r
voteint
```

    Item (measurement: nominal, type: double, length = 200) 

     [1:200] Other LibDem NoVote Lab DK N.a. NoVote NoVote Cons Cons LibDem ...

``` r
str(voteint)
```

     Nmnl. item w/ 8 labels for 1,2,3,...  num [1:200] 4 3 9 2 97 99 9 9 1 1 ...

### Missing values

In the coding plan shown above, the values 97, 98, and 99 are marked as
“missing values”, that is, while they represent coded responses, they
are not to be considered as valid in the sense of providing information
about the respondent’s vote intention. For the statistical analysis of
vote intention it is natural to replace them by `NA`. Yet replacing
codes 97, 98, and 99 already at the stage of importing data into *R*
memory would mean a loss of potentially precious information since it
precludes, e.g. the motivation to refuse responding to the vote
intention question or the antencedents of undecidedness. Hence it is
better to mark those values and to delay their replacement by `NA` to a
later stage in the analysis of vote intentions and to be able to undo or
change the “missingness” of these values. For example, not only may one
be interested in the antecedents of response refusals but also be
interested to analyse vote intention with non-voting excluded or
included. The memisc package provides, like SPSS and PSPP, facilities to
mark particular values of an item vector as “missing” and change such
designations throughout the data preperation stage.

There are several ways with `"memisc"` to make distinctions between
valid and missing values. The first way that mirrors the way it is done
in SPSS. To illustrate this we return to the fictitious vote intention
example. The values 97,98,99 of `voteint` are designated as “missing” by

``` r
missing.values(voteint) <- c(97,98,99)
```

The missing values are reflected in the output of `voteint`, (labels of)
missing values are marked with `*` in the output:

``` r
voteint
```

    Item (measurement: nominal, type: double, length = 200) 

     [1:200] Other LibDem NoVote Lab *DK *N.a. NoVote NoVote Cons Cons LibDem ...

It is also possible to extend the set of missing values: We add another
value to the set of missing values.

``` r
missing.values(voteint) <- missing.values(voteint) + 9
```

The missing values can be recalled as usual:

``` r
missing.values(voteint)
```

    97, 98, 99, 9

The missing values are turned into `NA` if `voteint` is coerced into a
numeric vector or a factor, which is what usually happens before the
eventual statistical analysis:

``` r
as.numeric(voteint)[1:30]
```

     [1]  4  3 NA  2 NA NA NA NA  1  1  3  3 NA  3 NA  1  1 NA NA  3  1 NA  1 NA NA
    [26] NA NA NA NA  2

``` r
as.factor(voteint)[1:30]
```

     [1] Other  LibDem <NA>   Lab    <NA>   <NA>   <NA>   <NA>   Cons   Cons  
    [11] LibDem LibDem <NA>   LibDem <NA>   Cons   Cons   <NA>   <NA>   LibDem
    [21] Cons   <NA>   Cons   <NA>   <NA>   <NA>   <NA>   <NA>   <NA>   Lab   
    Levels: Cons Lab LibDem Other

It is also possible to drop all missing value designations:

``` r
missing.values(voteint) <- NULL
missing.values(voteint)
```

    NULL

``` r
as.numeric(voteint)[1:30]
```

     [1]  4  3  9  2 97 99  9  9  1  1  3  3  9  3  9  1  1  9  9  3  1  9  1  9  9
    [26]  9 98 99  9  2

In contrast to SPSS it is possible with `"memisc"` to designate the
valid, i.e. non-missing values:

``` r
valid.values(voteint) <- 1:4
valid.values(voteint)
```

    1, 2, 3, 4

``` r
missing.values(voteint)
```

    9, 97, 98, 99

Instead of individual valid or missing values it is also possible to
define a *range* of values as valid:

``` r
valid.range(voteint) <- c(1,9)
missing.values(voteint)
```

    97, 98, 99

### Other attributes of survey items

Other software packages targeted at social scientists also allow to add
annotations to the variables in a data set, which are not subject to the
syntactic constraints of variable names. These annotations are usually
called “variable labels” in these software packages. In `"memisc"` the
corresponding term is “description”. In continuation of the running
example, we add a description to the vote intention variable:

``` r
description(voteint) <- "Vote intention"
description(voteint)
```

    [1] "Vote intention"

In contrast to other software, `"memisc"` allows to attach arbitrarily
annotation to survey items, such as the wording of a survey question:

``` r
wording(voteint) <- "Which party are you going to vote for in the general election next Tuesday?"
wording(voteint)
```

    [1] "Which party are you going to vote for in the general election next Tuesday?"

``` r
annotation(voteint)
```

    description:
         Vote intention

    wording:
         Which party are you going to vote for in the general election next
         Tuesday?

``` r
annotation(voteint)["wording"]
```

                                                                          wording 
    "Which party are you going to vote for in the general election next Tuesday?" 

### Codebooks of survey items

It is common in survey research to describe a data set in the form of a
*codebook*. A codebook summarises each variable in the data set in terms
of its relevant attributes, that is, the label attached to the variable
(in the context of the `memisc` package this is called its
“description”), the labels attached to the values of the variable, which
values of the variable are supposed to be *missing* or *valid*, as well
as univariate summary statistics of each variable, usually without and
with missing variables included. Such functionality is provided in this
package by the function
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md).
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md)
when applied to an `"item"` object returns a `"codebook"` object, which
when printed to the console gives an overview of the variable usually
required for the codebook of a data set (the production of codebooks for
whole data sets is described further below). To illustrate the
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md)
function we now produce a codebook of the `voteint` item variable
created above:

``` r
codebook(voteint)
```

    ================================================================================

       voteint 'Vote intention'

       "Which party are you going to vote for in the general election next
       Tuesday?"

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: nominal
       Valid range: 1 - 9

       Values and labels     N Valid Total
                                          
        1   'Cons'          49  27.8  24.5
        2   'Lab'           26  14.8  13.0
        3   'LibDem'        21  11.9  10.5
        4   'Other'         19  10.8   9.5
        9   'NoVote'        61  34.7  30.5
       97 M 'DK'             6         3.0
       98 M 'Refused'        7         3.5
       99 M 'N.a.'          11         5.5

As can be seen in the output, the
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md)
function reports the name of the variable, the description (if defined
for the variable), and the question wording (again if defined). Further
it reports the storage mode (which is use by *R*), the level of
measurement (“nominal”, “ordinal”, “interval”, or “ratio”) and the range
of valid values (or alternatively, individually defined valid values,
individually defined missing values, or ranges of missing values). For
item variables with value labels, it shows a table of frequencies of the
labelled values, and the percentages of valid values and all values with
missings included.

Codebooks are particularly useful to find “wild codes”, that is codes
that are not labelled, and usually produced by coding errors. Such
coding errors may be less common in data sets produced by CAPI or CATI
or online surveys, but they may occur in older data sets from before the
age of computer-assisted interviewing and also during the course of data
management. This use of codebooks is demonstrated in the following by
deliberatly adding some coding errors into a copy of our `voteint`
variable:

``` r
voteint1 <- voteint
voteint1[sample(length(voteint),size=20)] <- c(rep(5,13),rep(7,7))
```

The presence of these “wild codes” can now be spotted using
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md):

``` r
codebook(voteint1)
```

    ================================================================================

       voteint1 'Vote intention'

       "Which party are you going to vote for in the general election next
       Tuesday?"

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: nominal
       Valid range: 1 - 9

       Values and labels     N Valid Total
                                          
        1   'Cons'          44  25.0  22.0
        2   'Lab'           24  13.6  12.0
        3   'LibDem'        16   9.1   8.0
        4   'Other'         17   9.7   8.5
        9   'NoVote'        55  31.2  27.5
       97 M 'DK'             6         3.0
       98 M 'Refused'        7         3.5
       99 M 'N.a.'          11         5.5
            (unlab.val.)    20  11.4  10.0

The output shows that 20 observations contain wild codes in this
variable. Why don’t we get a list of wild codes as part of the codebook?
The reason is that codebook is supposed also to work with continuous
variables that have thousands of unique, unlabelled values. Users
certainly will not like to see them all as part of a codebook.

In order to get a list of wild codes the development version of “memisc”
contains the function
[`wild.codes()`](https://melff.github.io/memisc/reference/wild.codes.md),
which we apply to the variable `voteint1`

``` r
wild.codes(voteint1)
```

      Counts Percent
    5   13.0     6.5
    7    7.0     3.5

We see that 6.5 and 3.5 percent of the observations have the wild codes
5 and 7.

To see how
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md)
works with variables without value labels, we create an unlabelled copy
of our `voteint` variable:

``` r
voteint2 <- voteint
labels(voteint2) <- NULL # This deletes all value labels
codebook(voteint2)
```

    ================================================================================

       voteint2 'Vote intention'

       "Which party are you going to vote for in the general election next
       Tuesday?"

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: nominal
       Valid range: 1 - 9

       Values               N Valid Total
                                         
           (unlab.val.)   176 100.0  88.0
         M (unlab.mss.)    24        12.0

Usually, variables without labelled values represent measures on an
interval or ratio scale. In that case, we do not want to see how many
unlabelled values there are, but we want to get some other statistics,
such as mean, variance, etc. To this purpose, we decleare the variable
`voteint2` to have an interval-scale level of measurement.[²](#fn2)

``` r
measurement(voteint2) <- "interval"
codebook(voteint2)
```

    ================================================================================

       voteint2 'Vote intention'

       "Which party are you going to vote for in the general election next
       Tuesday?"

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: interval
       Valid range: 1 - 9

       Values                 N Percent
                                       
         M (unlab.mss.)      24    12.0
                                       
            Min: 1.000                 
            Max: 9.000                 
           Mean: 4.483                 
       Std.Dev.: 3.413                 

For convenience of including them into word-processor documents, there
is also the possibility to export codebooks into HTML:

``` r
show_html(codebook(voteint))
```

`voteint` — ‘Vote intention’

“Which party are you going to vote for in the general election next
Tuesday?”

|               |         |
|---------------|---------|
| Storage mode: | double  |
| Measurement:  | nominal |
| Valid range:  | 1 - 9   |

|                   |     |           |     |       |     |     |       |     |     |
|-------------------|-----|-----------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |           | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘Cons’    | 49  | 27    | .   | 8   | 24    | .   | 5   |
| 2                 |     | ‘Lab’     | 26  | 14    | .   | 8   | 13    | .   | 0   |
| 3                 |     | ‘LibDem’  | 21  | 11    | .   | 9   | 10    | .   | 5   |
| 4                 |     | ‘Other’   | 19  | 10    | .   | 8   | 9     | .   | 5   |
| 9                 |     | ‘NoVote’  | 61  | 34    | .   | 7   | 30    | .   | 5   |
| 97                | M   | ‘DK’      | 6   |       |     |     | 3     | .   | 0   |
| 98                | M   | ‘Refused’ | 7   |       |     |     | 3     | .   | 5   |
| 99                | M   | ‘N.a.’    | 11  |       |     |     | 5     | .   | 5   |

## Data sets: Containers of survey items

Usually one expects to be able handle data on responses to survey items
not in isolation, but as part of a data set, which contains a multitude
of observations on many variables. The usual data structure in *R* to
contain observation-on-variables data is the *data frame*. In principle
it is possible to put survey item vectors as described above into a data
frame, nevertheless the `"memisc"` package provides a special data
structure to contain survey item data called data sets or data
set-objects, that is, objects of class `"data.set"`. This opens up the
possibility to automatically translate survey items into regular vectors
and factors, as expected by typical data analysis functions, such as
[`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html).

### The structure of `"data.set"` objects

Data set objects have essentially the same row-by-column structure as
data frames: They are a set of vectors (however of class `"item"`) all
with the same length, so that in each row of the data set there are
values in these vectors. Observations can be addressed as rows of a
`"data.set"` and variabels can be addressed as columns, just as one may
used to with regards to data frames. Most data management operations
that you can do with data frames can also be done with data sets (such
as merging them or using the functions
[`with()`](https://rdrr.io/r/base/with.html) or
[`within()`](https://rdrr.io/r/base/with.html)). Yet in contrast to data
frames, data sets are always expected to contain objects of class
`"item"`, and any vectors or factors from which a `"data.set"` object is
constructed are changed into `"item"` objects.

Another difference is the way that `"data.set"` objects are shown on the
console. As `S4` objects, if a user types in the name of a `"data.set"`
objects, the function [`show()`](https://rdrr.io/r/methods/show.html)
(and not [`print()`](https://rdrr.io/r/base/print.html)) is applied to
it. The [`show()`](https://rdrr.io/r/methods/show.html)-method for data
set objects is defined in such a way that only the first few
observations of the first few variables are shown on the console – in
contrast to [`print()`](https://rdrr.io/r/base/print.html) as applied to
a data frame, which shows *all* observations on *all* variables. While
it may be intuitive and convenient to be shown all observations in a
small data frame, this is not what you will want if your data set
contains more than 2000 observations on several hundred variables, the
dimensions that typical social science data sets have that you can
download from data archives such as that of ICPSR or GESIS.

The main facilitites of `"data.set"` objects are demonstrated in what
follows. First, we create a data set with fictional survey responses

``` r
Data <- data.set(
          vote = sample(c(1,2,3,4,8,9,97,99),
                        size=300,replace=TRUE),
          region = sample(c(rep(1,3),rep(2,2),3,99),
                          size=300,replace=TRUE),
          income = round(exp(rnorm(300,sd=.7))*2000)
          )
```

Then, we take a look at this already sizeable `"data.set"`” object:

``` r
Data
```

    Data set with 300 observations and 3 variables

       vote region income
     1    2      3   4950
     2   99     99    727
     3    2      3   1667
     4   97     99   2970
     5    1      1   2943
     6    9      2   1351
     7    1      1   1540
     8    4      1   2270
     9    3      1   2047
    10    8      1   6042
    11    9     99   1589
    12    3     99   5126
    13    1      1   1206
    14    8      2   8878
    15    8      1   2859
    16    3      1   1038
    17    2      2   1844
    18    2      1   2928
    19    9     99    921
    20   97      1   2885
    21    1      2   1453
    22    4      3   1185
    23    8      2   3593
    24    2      3   4981
    25    2      2   8243
    .. .... ...... ......
    (25 of 300 observations shown)

In this case, our data set has only three variables, all of which are
shown, but of the observations we see only the first 25. Actually the
number of observations shown can be determined by the option
`"show.max.obs"` which defaults to 25, but can be changed as convenient:

``` r
options(show.max.obs=5)
Data
```

    Data set with 300 observations and 3 variables

      vote region income
    1    2      3   4950
    2   99     99    727
    3    2      3   1667
    4   97     99   2970
    5    1      1   2943
    . .... ...... ......
    (5 of 300 observations shown)

``` r
# Back to the default
options(show.max.obs=25)
```

If you *really* want to see the complete data on your console, then you
can use [`print()`](https://rdrr.io/r/base/print.html) instead:

``` r
print(Data)
```

but you should not do this with large data sets, such as the
Eurobarometer trend file …

### Manipulating data in data sets

Typical data management tasks that you would otherwise have done in
commercial packages like SPSS or Stata can be conducted within data set
objects. Actually to provide this possibility (to the author of the
package) was the main reason that the `"memisc"` package was created. To
demonstrate this, we continue with our fictional data which we prepare
for further analysis:

``` r
Data <- within(Data,{
  description(vote) <- "Vote intention"
  description(region) <- "Region of residence"
  description(income) <- "Household income"
  wording(vote) <- "If a general election would take place next Tuesday,
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
                    "Other"               =  4,
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
```

    Warning in within(Data, {: Variables 'junk1','junk2' have wrong length,
    removing them.

Now that we have added information to the data set that reflects the
code plan of the variables, we take a look how the it looks like:

``` r
Data
```

    Data set with 300 observations and 3 variables

                       vote               region income
     1               Labour                Wales   4950
     2 *Not asked in survey *Not asked in survey    727
     3               Labour                Wales   1667
     4      *Not applicable *Not asked in survey   2970
     5        Conservatives              England   2943
     6      *Answer refused             Scotland   1351
     7        Conservatives              England   1540
     8                Other              England   2270
     9    Liberal Democrats              England   2047
    10          *Don't know              England   6042
    11      *Answer refused *Not asked in survey   1589
    12    Liberal Democrats *Not asked in survey   5126
    13        Conservatives              England   1206
    14          *Don't know             Scotland   8878
    15          *Don't know              England   2859
    16    Liberal Democrats              England   1038
    17               Labour             Scotland   1844
    18               Labour              England   2928
    19      *Answer refused *Not asked in survey    921
    20      *Not applicable              England   2885
    21        Conservatives             Scotland   1453
    22                Other                Wales   1185
    23          *Don't know             Scotland   3593
    24               Labour                Wales   4981
    25               Labour             Scotland   8243
    .. .................... .................... ......
    (25 of 300 observations shown)

As you can see, labelled item look a bit like factors, but with a
difference: User-defined missing values are marked with an asterisk.

Subsetting a data set object works as expected:

``` r
EnglandData <- subset(Data,region == "England")
EnglandData
```

    Data set with 132 observations and 3 variables

                       vote  region income
     1        Conservatives England   2943
     2        Conservatives England   1540
     3                Other England   2270
     4    Liberal Democrats England   2047
     5          *Don't know England   6042
     6        Conservatives England   1206
     7          *Don't know England   2859
     8    Liberal Democrats England   1038
     9               Labour England   2928
    10      *Not applicable England   2885
    11                Other England   2155
    12                Other England   1280
    13      *Not applicable England   4111
    14               Labour England    689
    15 *Not asked in survey England   2421
    16                Other England   5511
    17 *Not asked in survey England   4628
    18          *Don't know England    896
    19          *Don't know England    842
    20          *Don't know England    948
    21        Conservatives England   2346
    22        Conservatives England   1234
    23                Other England   1186
    24        Conservatives England   1215
    25      *Not applicable England   5516
    .. .................... ....... ......
    (25 of 132 observations shown)

### Codebooks of data sets

Previouly, we created a code book for individual survey items. But it is
also possible to create a codebook for a whole data set (what one
usually wants to have a codebook of). Obtaining a codebook is simple, by
applying the function
[`codebook()`](https://melff.github.io/memisc/reference/codebook.md) to
the data frame:

``` r
codebook(Data)
```

    ================================================================================

       vote 'Vote intention'

       "If a general election would take place next Tuesday, the candidate of which
       party would you vote for?"

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: nominal
       Missing values: 8, 9, 97, 99

       Values and labels              N Valid Total
                                                   
        1   'Conservatives'          32  21.1  10.7
        2   'Labour'                 41  27.0  13.7
        3   'Liberal Democrats'      36  23.7  12.0
        4   'Other'                  43  28.3  14.3
        8 M 'Don't know'             47        15.7
        9 M 'Answer refused'         29         9.7
       97 M 'Not applicable'         28         9.3
       99 M 'Not asked in survey'    44        14.7

       Remark:
           This is not a real survey item, of course ...

    ================================================================================

       region 'Region of residence'

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: nominal
       Missing values: 97, 99

       Values and labels              N Valid Total
                                                   
        1   'England'               132  51.4  44.0
        2   'Scotland'               87  33.9  29.0
        3   'Wales'                  38  14.8  12.7
       99 M 'Not asked in survey'    43        14.3

       Remark:
           This is not a real survey item, of course ...

    ================================================================================

       income 'Household income'

       "All things taken into account, how much do all household members earn in
       sum?"

    --------------------------------------------------------------------------------

       Storage mode: double
       Measurement: ratio

            Min:   245.000
            Max: 13596.000
           Mean:  2556.743
       Std.Dev.:  2158.757

       Remark:
           This is not a real survey item, of course ...

On a website, it looks better in HTML:

``` r
show_html(codebook(Data))
```

`vote` — ‘Vote intention’

“If a general election would take place next Tuesday, the candidate of
which party would you vote for?”

|                 |              |
|-----------------|--------------|
| Storage mode:   | double       |
| Measurement:    | nominal      |
| Missing values: | 8, 9, 97, 99 |

|                   |     |                       |     |       |     |     |       |     |     |
|-------------------|-----|-----------------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                       | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘Conservatives’       | 32  | 21    | .   | 1   | 10    | .   | 7   |
| 2                 |     | ‘Labour’              | 41  | 27    | .   | 0   | 13    | .   | 7   |
| 3                 |     | ‘Liberal Democrats’   | 36  | 23    | .   | 7   | 12    | .   | 0   |
| 4                 |     | ‘Other’               | 43  | 28    | .   | 3   | 14    | .   | 3   |
| 8                 | M   | ‘Don’t know’          | 47  |       |     |     | 15    | .   | 7   |
| 9                 | M   | ‘Answer refused’      | 29  |       |     |     | 9     | .   | 7   |
| 97                | M   | ‘Not applicable’      | 28  |       |     |     | 9     | .   | 3   |
| 99                | M   | ‘Not asked in survey’ | 44  |       |     |     | 14    | .   | 7   |

- Remark: : This is not a real survey item, of course …

`region` — ‘Region of residence’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 97, 99  |

|                   |     |                       |     |       |     |     |       |     |     |
|-------------------|-----|-----------------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                       | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘England’             | 132 | 51    | .   | 4   | 44    | .   | 0   |
| 2                 |     | ‘Scotland’            | 87  | 33    | .   | 9   | 29    | .   | 0   |
| 3                 |     | ‘Wales’               | 38  | 14    | .   | 8   | 12    | .   | 7   |
| 99                | M   | ‘Not asked in survey’ | 43  |       |     |     | 14    | .   | 3   |

- Remark: : This is not a real survey item, of course …

`income` — ‘Household income’

“All things taken into account, how much do all household members earn
in sum?”

|               |        |
|---------------|--------|
| Storage mode: | double |
| Measurement:  | ratio  |

|           |       |     |     |
|-----------|-------|-----|-----|
| Min:      | 245   | .   | 000 |
| Max:      | 13596 | .   | 000 |
| Mean:     | 2556  | .   | 743 |
| Std.Dev.: | 2158  | .   | 757 |

- Remark: : This is not a real survey item, of course …

### Translating data sets into data frames

The punchline of the existence of `"data.set"` objects however is that
they can be coerced into regular data frames, using
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), which
causes survey items to be translated into regular numeric vectors or
factors using [`as.numeric()`](https://rdrr.io/r/base/numeric.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html) or
[`as.ordered()`](https://rdrr.io/r/base/factor.html) as above, and
pre-determined missing values changed into `NA`. Whether a survey item
is changed into a numerical vector, an unordered or an ordered factor
depends on the declared measurement level (which can be manipulated by
[`measurement()`](https://melff.github.io/memisc/reference/measurement.md)
as shown above).

In the example developed so far, the variables `vote` and `region` are
declared to have a nominal level of measurement, while `income` is
declared to have a ratio scale. That is, in statistical analyses, we
want the first two variables to be handled as (unordered) factors, and
the income variable as a numerical vector. In addition, we want all the
user-declared missing values to be changed into `NA` so that
observations where respondents stated to “don’t know” what they are
goint go vote for are excluded from the analysis. So let’s see whether
this works - we coerce our data set into a data frame:

``` r
DataFr <- as.data.frame(Data)
## Looking a the data frame structure
str(DataFr)
```

    'data.frame':   300 obs. of  3 variables:
     $ vote  : Factor w/ 4 levels "Conservatives",..: 2 NA 2 NA 1 NA 1 4 3 NA ...
      ..- attr(*, "label")= chr "Vote intention"
     $ region: Factor w/ 3 levels "England","Scotland",..: 3 NA 3 NA 1 2 1 1 1 1 ...
      ..- attr(*, "label")= chr "Region of residence"
     $ income: num  4950 727 1667 2970 2943 ...
      ..- attr(*, "label")= chr "Household income"

``` r
## Looking at the first 25 observations
DataFr[1:25,]
```

                    vote   region income
    1             Labour    Wales   4950
    2               <NA>     <NA>    727
    3             Labour    Wales   1667
    4               <NA>     <NA>   2970
    5      Conservatives  England   2943
    6               <NA> Scotland   1351
    7      Conservatives  England   1540
    8              Other  England   2270
    9  Liberal Democrats  England   2047
    10              <NA>  England   6042
    11              <NA>     <NA>   1589
    12 Liberal Democrats     <NA>   5126
    13     Conservatives  England   1206
    14              <NA> Scotland   8878
    15              <NA>  England   2859
    16 Liberal Democrats  England   1038
    17            Labour Scotland   1844
    18            Labour  England   2928
    19              <NA>     <NA>    921
    20              <NA>  England   2885
    21     Conservatives Scotland   1453
    22             Other    Wales   1185
    23              <NA> Scotland   3593
    24            Labour    Wales   4981
    25            Labour Scotland   8243

Indeed the translation works as expected, so we can use it for
statistical analysis, here a simple cross tab:

``` r
xtabs(~vote+region,data=DataFr)
```

                       region
    vote                England Scotland Wales
      Conservatives          16        4     7
      Labour                 12       17     7
      Liberal Democrats      20        7     3
      Other                  24       13     4

In fact, since many functions such as
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html),
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html), etc. coerce theire `data=`
argument into a data frame, an explicit coercion with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) is not
always needed:

``` r
xtabs(~vote+region,data=Data)
```

                       region
    vote                England Scotland Wales
      Conservatives          16        4     7
      Labour                 12       17     7
      Liberal Democrats      20        7     3
      Other                  24       13     4

Sometimes we do want missing values to be included, and this is possible
too:

``` r
xtabs(~vote+region,data=within(Data, 
                               vote <- include.missings(vote)))
```

                          region
    vote                   England Scotland Wales
      Conservatives             16        4     7
      Labour                    12       17     7
      Liberal Democrats         20        7     3
      Other                     24       13     4
      *Don't know               19       19     4
      *Answer refused           11       10     3
      *Not applicable           12        5     6
      *Not asked in survey      18       12     4

For convenience, there is also a codebook method for data frames:

``` r
show_html(codebook(DataFr))
```

`vote` — ‘Vote intention’

|               |          |
|---------------|----------|
| Storage mode: | integer  |
| Factor with   | 4 levels |

|                   |     |                     |     |       |     |     |       |     |     |
|-------------------|-----|---------------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                     | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘Conservatives’     | 32  | 21    | .   | 1   | 10    | .   | 7   |
| 2                 |     | ‘Labour’            | 41  | 27    | .   | 0   | 13    | .   | 7   |
| 3                 |     | ‘Liberal Democrats’ | 36  | 23    | .   | 7   | 12    | .   | 0   |
| 4                 |     | ‘Other’             | 43  | 28    | .   | 3   | 14    | .   | 3   |
|                   |     | NA                  | 148 |       |     |     | 49    | .   | 3   |

`region` — ‘Region of residence’

|               |          |
|---------------|----------|
| Storage mode: | integer  |
| Factor with   | 3 levels |

|                   |     |            |     |       |     |     |       |     |     |
|-------------------|-----|------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |            | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘England’  | 132 | 51    | .   | 4   | 44    | .   | 0   |
| 2                 |     | ‘Scotland’ | 87  | 33    | .   | 9   | 29    | .   | 0   |
| 3                 |     | ‘Wales’    | 38  | 14    | .   | 8   | 12    | .   | 7   |
|                   |     | NA         | 43  |       |     |     | 14    | .   | 3   |

`income` — ‘Household income’

|               |        |
|---------------|--------|
| Storage mode: | double |

|           |       |     |     |
|-----------|-------|-----|-----|
| Min:      | 245   | .   | 000 |
| Max:      | 13596 | .   | 000 |
| Mean:     | 2556  | .   | 743 |
| Std.Dev.: | 2158  | .   | 757 |

## More tools for data preparation

When social scientists work with survey data, these are not always
organised and coded in a way that suits the intended data analysis. For
this reasons, the `"memisc"` package provides the two functions
[`recode()`](https://melff.github.io/memisc/reference/recode.md) and
[`cases()`](https://melff.github.io/memisc/reference/cases.md). The
former is – as the name suggests – for recoding, while the second allows
for complex distinctions of cases and can be seen as a more general
version of [`ifelse()`](https://rdrr.io/r/base/ifelse.html). These two
functions are demonstrated with a “real-life” example.

### Recoding

The function
[`recode()`](https://melff.github.io/memisc/reference/recode.md) is
similar in semantics to the function of the same name in package
[`"car"`](https://cran.r-project.org/package=car) and designed in such a
way that it does not conflict with this function. In fact, if
[`recode()`](https://melff.github.io/memisc/reference/recode.md) is
called in the way as expected in package `"car"`, it will dispatch
processing to this function. In other words, users of this other package
may use [`recode()`](https://melff.github.io/memisc/reference/recode.md)
as they are used to. The version of the
[`recode()`](https://melff.github.io/memisc/reference/recode.md)
function provided by `"memisc"` differs from the `"car"` version in so
far as its syntax is more R-ish (or so I believe).

Here we load an example data set – a subset of the German Longitudinal
Election Study for 2013[³](#fn3) – into R’s memory.

``` r
load(system.file("gles/gles2013work.RData",package="memisc"))
```

As a simple example for the use of
[`recode()`](https://melff.github.io/memisc/reference/recode.md) we use
this function to recode German Bundesländer into an item with two values
or East and West Germany. But first we create a codebook for the
variable that contains the Bundesländer codes:

``` r
with(gles2013work,
     show_html(codebook(bula)))
```

`bula` — ‘Bundesland’

|               |         |
|---------------|---------|
| Storage mode: | double  |
| Measurement:  | nominal |

|                   |     |                          |     |         |     |     |
|-------------------|-----|--------------------------|-----|---------|-----|-----|
| Values and labels |     |                          | N   | Percent |     |     |
| 1                 |     | ‘Baden-Wuerttemberg’     | 333 | 8       | .   | 5   |
| 2                 |     | ‘Bayern’                 | 507 | 13      | .   | 0   |
| 3                 |     | ‘Berlin’                 | 190 | 4       | .   | 9   |
| 4                 |     | ‘Brandenburg’            | 212 | 5       | .   | 4   |
| 5                 |     | ‘Bremen’                 | 27  | 0       | .   | 7   |
| 6                 |     | ‘Hamburg’                | 49  | 1       | .   | 3   |
| 7                 |     | ‘Hessen’                 | 232 | 5       | .   | 9   |
| 8                 |     | ‘Mecklenburg-Vorpommern’ | 160 | 4       | .   | 1   |
| 9                 |     | ‘Niedersachsen’          | 331 | 8       | .   | 5   |
| 10                |     | ‘Nordrhein-Westfalen’    | 619 | 15      | .   | 8   |
| 11                |     | ‘Rheinland-Pfalz’        | 150 | 3       | .   | 8   |
| 12                |     | ‘Saarland’               | 45  | 1       | .   | 2   |
| 13                |     | ‘Sachsen’                | 402 | 10      | .   | 3   |
| 14                |     | ‘Sachsen-Anhalt’         | 252 | 6       | .   | 4   |
| 15                |     | ‘Schleswig-Holstein’     | 131 | 3       | .   | 3   |
| 16                |     | ‘Thueringen’             | 271 | 6       | .   | 9   |

We now recode the Bundesländer codes into a new variable:

``` r
gles2013work <- within(gles2013work,
                       east.west <- recode(bula,
                                          East = 1 <- c(3,4,8,13,14,16),
                                          West = 2 <- c(1,2,5:7,9:12,15)
                                          ))
```

and check whether this was successful:

``` r
xtabs(~bula+east.west,data=gles2013work)
```

                            east.west
    bula                     East West
      Baden-Wuerttemberg        0  333
      Bayern                    0  507
      Berlin                  190    0
      Brandenburg             212    0
      Bremen                    0   27
      Hamburg                   0   49
      Hessen                    0  232
      Mecklenburg-Vorpommern  160    0
      Niedersachsen             0  331
      Nordrhein-Westfalen       0  619
      Rheinland-Pfalz           0  150
      Saarland                  0   45
      Sachsen                 402    0
      Sachsen-Anhalt          252    0
      Schleswig-Holstein        0  131
      Thueringen              271    0

as can be seen,
[`recode()`](https://melff.github.io/memisc/reference/recode.md) was
called in such a way that not only old codes are transferred into new
ones, but also the new codes are labelled.

### Case distinctions

Recoding can be used to combine the codes of an item into a smaller set,
but sometimes one needs to do more complex data preparations, in which
the values of some variable are set conditional on values of another
one, etc. For such tasks, the `"memisc"` package provides the function
[`cases()`](https://melff.github.io/memisc/reference/cases.md). This
function takes several expressions that evaluate to logical vectors as
arguments and returns a numeric vector or a factor, the values or level
of which indicate for each observation which of the expressions
evaluates to `TRUE` the respective observation. The factor levels are
named after the logical expressions. A simple example looks thus:

``` r
x <- 1:10
xc <- cases(x <= 3, 
            x > 3 & x <= 7, 
            x > 7)
data.frame(x,xc)
```

        x             xc
    1   1         x <= 3
    2   2         x <= 3
    3   3         x <= 3
    4   4 x > 3 & x <= 7
    5   5 x > 3 & x <= 7
    6   6 x > 3 & x <= 7
    7   7 x > 3 & x <= 7
    8   8          x > 7
    9   9          x > 7
    10 10          x > 7

In this example
[`cases()`](https://melff.github.io/memisc/reference/cases.md) returns a
factor. It can also be made to return a numeric value:

``` r
xn <- cases(1 <- x <= 3, 
            2 <- x > 3 & x <= 7, 
            3 <- x > 7)
data.frame(x,xn)
```

        x xn
    1   1  1
    2   2  1
    3   3  1
    4   4  2
    5   5  2
    6   6  2
    7   7  2
    8   8  3
    9   9  3
    10 10  3

This example shows the way
[`cases()`](https://melff.github.io/memisc/reference/cases.md) works in
the abstract. How this can be made used of in practical example is best
demonstrated by a real-world example, again using data from the German
Longitudinal Election Study.

In the 2013 election module, the intention to vote during the
pre-election of respondents interviewed in the pre-election wave
(`wave==1`) and the participation in the election of respondents
interviewed in the post-election wave (`wave==2`) are recorded in
different data set variables, named here `intent.turnout` and `turnout`.
The variable `intent.voteint` has codes for whether the respondents were
sure to participate (1), were likely to participate (2), were undecided
(3), likely not to (4), sure not to participate (5), or whether they
have cast a postal vote (6). Variable `turnout` has codes for those who
participated in the election (1) or did not (2).

The intention for the candidate vote is recorded in variable
`voteint.candidate` and the intention for the list vote is recoded in
variable `voteint.list` for the pre-election wave. A postal vote for
party candidate is recorded in variable `postal.vote.candidate` and for
a party list is in variable `postal.vote.list`. Recalled votes in the
post-election wave are recorded in variables `vote.candidate` and
`vote.list`.

These various variables are combined into two variables that has valid
values for both waves, `candidate.vote` and `list.vote`. For this,
several conditions have to be handled: whether a respondent is in the
pre-election or the post-election wave, whether s/he is not likely or
sure not to vote, or whether she has cast a postal vote. Thus the
variable [`cases()`](https://melff.github.io/memisc/reference/cases.md)
is helpful here:

``` r
gles2013work <- within(gles2013work,{

  candidate.vote <- cases(
              wave == 1 & intent.turnout == 6 -> postal.vote.candidate,
              wave == 1 & intent.turnout %in% 4:5 -> 900,
              wave == 1 & intent.turnout %in% 1:3 -> voteint.candidate,
              wave == 2 & turnout == 1 -> vote.candidate,
              wave == 2 & turnout == 2 -> 900
            )

  list.vote <- cases(
              wave == 1 & intent.turnout == 6 -> postal.vote.list,
              wave == 1 & intent.turnout %in% 4:5 -> 900,
              wave == 1 & intent.turnout %in% 1:3 -> voteint.list,
              wave == 2 & turnout ==1 -> vote.list,
              wave == 2 & turnout ==2 -> 900
            )
})
```

    Warning in cases(postal.vote.candidate <- wave == 1 & intent.turnout == : 78
    NAs created

    Warning in cases(postal.vote.list <- wave == 1 & intent.turnout == 6, 900 <-
    wave == : 78 NAs created

The code shown above does the following: In the pre-election wave
(`wave == 1`), the `candidate.vote` variable receives the value of the
postal vote variable `postal.vote.candidate` if a postal vote was cast
(`intent.turnout == 6`), it receives the value `900` for those
respondents who where likely or sure not to vote
(`intent.turnout %in% 4:5`), and the value of the variable
`voteint.candidate` for all others (`intent.turnout %in% 1:3`). In the
post-election wave (`wave == 2`) variable `candidate.vote` receives the
value of variable `vote.candidate` if the respondent has voted
(`turnout == 1`) or the value `900` if s/he has not voted
(`turnout == 2`). The variable `list.vote` is constructed in an
analogous manner from the variables `wave`, `intent.turnout`, `turnout`,
`postal.vote.list`, `voteint.list` and `vote.list`. After the
constructin, the resulting variables `candidate.vote` and `list.vote`
are labelled and missing values are declared:

``` r
gles2013work <- within(gles2013work,{
  candidate.vote <- recode(as.item(candidate.vote),
                      "CDU/CSU"   =  1 <- 1,
                      "SPD"       =  2 <- 4,
                      "FDP"       =  3 <- 5,
                      "Grüne"     =  4 <- 6,
                      "Linke"     =  5 <- 7,
                      "NPD"       =  6 <- 206,
                      "Piraten"   =  7 <- 215,
                      "AfD"       =  8 <- 322,
                      "Other"     = 10 <- 801,
                      "No Vote"   = 90 <- 900,
                      "WN"        = 98 <- -98,
                      "KA"        = 99 <- -99
                  )
  list.vote <- recode(as.item(list.vote),
                      "CDU/CSU"   =  1 <- 1,
                      "SPD"       =  2 <- 4,
                      "FDP"       =  3 <- 5,
                      "Grüne"     =  4 <- 6,
                      "Linke"     =  5 <- 7,
                      "NPD"       =  6 <- 206,
                      "Piraten"   =  7 <- 215,
                      "AfD"       =  8 <- 322,
                      "Other"     = 10 <- 801,
                      "No Vote"   = 90 <- 900,
                      "WN"        = 98 <- -98,
                      "KA"        = 99 <- -99
                  )
  
   missing.values(candidate.vote) <- 98:99
   missing.values(list.vote) <- 98:99
   measurement(candidate.vote) <- "nominal"
   measurement(list.vote) <- "nominal"
})
```

    Warning in recode(as.item(candidate.vote), `CDU/CSU` = 1 <- 1, SPD = 2 <- 4, :
    recoding created 18 NAs

    Warning in recode(as.item(list.vote), `CDU/CSU` = 1 <- 1, SPD = 2 <- 4, :
    recoding created 19 NAs

Finally, we can get a cross-tabulation of list votes and the East-West
factor and a cross tabulation of candidate votes against list votes:

``` r
xtabs(~list.vote+east.west,data=gles2013work)
```

             east.west
    list.vote East West
      CDU/CSU  440  714
      SPD      268  554
      FDP       32   87
      Grüne     70  226
      Linke    227  101
      NPD       11    6
      Piraten   14   34
      AfD       27   63
      Other      6   21
      No Vote  197  318

``` r
xtabs(~list.vote+candidate.vote,data=gles2013work)
```

             candidate.vote
    list.vote CDU/CSU  SPD  FDP Grüne Linke  NPD Piraten  AfD Other No Vote
      CDU/CSU    1060   29   20     3    12    0       2    0     2       0
      SPD          44  700    1    39    14    1       2    1     1       0
      FDP          67   13   33     1     0    0       2    0     0       0
      Grüne        32  102    4   141     7    0       5    3     0       0
      Linke        10   45    2    15   245    2       2    2     1       0
      NPD           0    2    0     0     1   12       0    0     1       0
      Piraten       3    3    1     8     5    0      25    1     0       0
      AfD          20    7    2     2     5    2       5   43     2       0
      Other         5    4    0     3     1    1       0    1    11       0
      No Vote       0    0    0     0     0    0       0    0     0     515

------------------------------------------------------------------------

1.  Those familiar with British politics will realise that this is a
    simplification of the menu of available choices that voters in
    England typically face in an election of the House of Commons.

2.  Of course, substantially it does not make sense at all to form
    averages etc. of voting choices, so “do not try this at home”. This
    example is merely to demonstrate codebooks and the setting of
    scale-levels.

3.  The [German Longitudinal Election
    Study](https://www.gesis.org/gles/about-gles) is funded by the
    German National Science Foundation (DFG) and carried out outin close
    cooperation with the [DGfW](https://www.dgfw.info/), German Society
    for Electoral Studies. Principal investigators are Hans Rattinger
    (University of Mannheim, until 2014), Sigrid Roßteutscher
    (University of Frankfurt), Rüdiger Schmitt-Beck (University of
    Mannheim), Harald Schoen (Mannheim Centre for European Social
    Research, from 2015), Bernhard Weßels (Social Science Research
    Center Berlin), and Christof Wolf (GESIS – Leibniz Institute for the
    Social Sciences, since 2012). Neither the funding organisation nor
    the principal investigators bear any responsibility for the example
    code shown here.
