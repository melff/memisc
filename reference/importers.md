# Object Oriented Interface to Foreign Files

Importer objects are objects that refer to an external data file.
Currently only Stata files, SPSS system, portable, and fixed-column
files are supported.

Data are actually imported by \`translating' an importer file into a
[`data.set`](https://melff.github.io/memisc/reference/dataSets.md) using
`as.data.set` or `subset`.

The `importer` mechanism is more flexible and extensible than
[`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html) and
[`read.dta`](https://rdrr.io/pkg/foreign/man/read.dta.html) of package
"foreign", as most of the parsing of the file headers is done in R. It
is also adapted to efficiently load large data sets. Most importantly,
importer objects support the
[`labels`](https://melff.github.io/memisc/reference/labels.md),
[`missing.values`](https://melff.github.io/memisc/reference/value-filters.md),
and
[`description`](https://melff.github.io/memisc/reference/annotations.md)s,
provided by this package.

## Usage

``` r
spss.file(file,...)

spss.fixed.file(file,
  columns.file,
  varlab.file=NULL,
  codes.file=NULL,
  missval.file=NULL,
  count.cases=TRUE,
  to.lower=getOption("spss.fixed.to.lower",FALSE),
  iconv=TRUE,
  encoded=getOption("spss.fixed.encoding","cp1252"),
  negative2missing = FALSE)

spss.portable.file(file,
  varlab.file=NULL,
  codes.file=NULL,
  missval.file=NULL,
  count.cases=TRUE,
  to.lower=getOption("spss.por.to.lower",FALSE),
  iconv=TRUE,
  encoded=getOption("spss.por.encoding","cp1252"),
  negative2missing = FALSE)

spss.system.file(file,
  varlab.file=NULL,
  codes.file=NULL,
  missval.file=NULL,
  count.cases=TRUE,
  to.lower=getOption("spss.sav.to.lower",FALSE),
  iconv=TRUE,
  encoded=getOption("spss.sav.encoding","cp1252"),
  ignore.scale.info = FALSE,
  negative2missing = FALSE)

Stata.file(file,
           iconv=TRUE,
           encoded=if(new_format)
                        getOption("Stata.new.encoding","utf-8")
                   else getOption("Stata.old.encoding","cp1252"),
           negative2missing = FALSE)

## The most important methods for "importer" objects are:
# S3 method for class 'spss.system.importer'
subset(x, subset, select, drop = FALSE, ...)
# S3 method for class 'spss.portable.importer'
subset(x, subset, select, drop = FALSE, ...)
# S3 method for class 'spss.fixed.importer'
subset(x, subset, select, drop = FALSE, ...)
# S3 method for class 'Stata.importer'
subset(x, subset, select, drop = FALSE, ...)
# S3 method for class 'Stata_new.importer'
subset(x, subset, select, drop = FALSE, ...)

# S4 method for class 'importer'
as.data.set(x,row.names=NULL,optional=NULL,
                    compress.storage.modes=FALSE,...)

# S4 method for class 'importer'
head(x,n=20,...)
# S4 method for class 'importer'
tail(x,n=20,...)
```

## Arguments

- file:

  character string; the path to the file containing the data

- ...:

  Other arguments. `spss.file()` passes them on to
  `spss.portable.file()` of `spss.system.file()`. Other function ignore
  further arguments.

- columns.file:

  character string; the path to an SPSS/PSPP syntax file with a
  `DATA LIST FIXED` statement

- varlab.file:

  character string; the path to an SPSS/PSPP syntax file with a
  `VARIABLE LABELS` statement

- codes.file:

  character string; the path to an SPSS/PSPP syntax file with a
  `VALUE LABELS` statement

- missval.file:

  character string; the path to an SPSS/PSPP syntax file with a
  `MISSING VALUES` statement

- count.cases:

  logical; should cases in file be counted? This takes effect only if
  the data file does not already contain information about the number of
  cases.

- to.lower:

  logical; should variable names changed to lower case?

- iconv:

  logical; should strings (in labels and variables) changed into
  encoding of the platform?

- encoded:

  a cacharacter string; the way characters are encoded in the improrted
  file. For the available encoding options see
  [`?iconvlist`](https://rdrr.io/r/base/iconv.html). Using this argument
  for `spss.system.file` this is only a fallback, as the function uses
  the encoding information present in the file if it is present.

- negative2missing:

  logical; should negative values be marked as missing values? This is
  the convention of some newer data sets that are available e.g. from
  the GESIS data archive.

- ignore.scale.info:

  logical; should information about measuremnt scale levels provided in
  the file be ignored?

- x:

  an object that inherits from class `"importer"`.

- subset:

  a logical vector or an expression containing variables from the
  external data file that evaluates to logical.

- select:

  a vector of variable names from the external data file. This may also
  be a named vector, where the names give the names into which the
  variables from the external data file are renamed.

- drop:

  a logical value, that determines what happens if only one column is
  selected. If TRUE and only one column is selected, `subset` returns
  only a single `item` object and not a `data.set`.

- row.names:

  ignored, present only for compatibility.

- optional:

  ignored, present only for compatibility.

- compress.storage.modes:

  logical value; if TRUE floating point values are converted to integers
  if possible without loss of information.

- n:

  integer; the number of rows to be shown by `head` or `tail`

## Value

`spss.fixed.file`, `spss.portable.file`, `spss.system.file`, and
`Stata.file` return, respectively, objects of class
`"spss.fixed.importer"`, `"spss.portable.importer"`,
`"spss.system.importer"`, `"Stata.importer"`, or `"Stata_new.importer"`,
which, by inheritance, are also objects of class `"importer"`.
`"Stata.importer"` is for files in the format of Stata versions up to
12, while `"Stata_new.importer"` is for files in the newer format of
Stata versions from 13.

Objects of class `"importer"` have at least the following two slots:

- ptr:

  an external pointer

- variables:

  a list of objects of class `"item.vector"` which provides a
  \`prototype' for the `"data.set"` set objects returned by the
  `as.data.set` and `subset` methods for objects of class `"importer"`

The `as.data.frame` for `importer` objects does the actual data import
and returns a data frame. Note that in contrast to
[`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html), the
variable names of the resulting data frame will be lower case, unless
the importer function is called with `to.lower=FALSE`. If long variable
names are defined (in case of a PSPP/SPSS system file), they take
precedence and are *not* coerced to lower case.

## See also

[`codebook`](https://melff.github.io/memisc/reference/codebook.md),
[`description`](https://melff.github.io/memisc/reference/annotations.md),
[`read.spss`](https://rdrr.io/pkg/foreign/man/read.spss.html)

## Details

A call to a \`constructor' for an importer object, that is,
`spss.fixed.file`, `spss.portable.file`, `spss.sysntax.file`, or
`Stata.file`, causes R to read in the header of the data file and/or the
syntax files that contain information about the variables, such as the
columns that they occupy (in case of `spss.fixed.file`), variable
labels, value labels and missing values.

The information in the file header and/or the accompagnying files is
then processed to prepare the file for importing. Thus the inner
structure of an `importer` object may well vary according to what type
of file is to imported and what additional information is given.

The `as.data.set` and `subset` methods for `"importer"` objects
internally use the generic functions `seekData`, `readData`,
`readSlice`, and `readChunk`, which have methods for the subclasses of
`"importer"`. These functions are not callable from outside the package,
however.

The `subset` method for `"importer"` objects reads in the data
\`chunk-wise' to create the subset of observations if the option
`"subset.chunk.size"` is set to a non-`NULL` value, e.g. by
`options(subset.chunk.size=1000)`. This may be useful in case of very
large data sets from which only a tiny subset of observations is needed
for analysis.

Since the functions described here are more or less complete rewrite
based on the description of the file structure provided by the
documenation for PSPP, they are perhaps not as thorougly tested as the
functions in the `foreign` package, apart from the frequent use by the
author of this package.

## Examples

``` r
# Extract American National Election Study of 1948
nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())

# Get information about the variables contained.
nes1948 <- spss.portable.file(nes1948.por)
#> Warning: 9 variables have duplicated labels:
#>   V480004, V480012, V480020, V480021A, V480021B, V480033A, V480033B,
#>   V480034A, V480034B

# The data are not yet loaded:
show(nes1948)
#> 
#> SPSS portable file '/tmp/RtmpGgTkFk/file1dce3748c478/NES1948.POR' 
#>  with 67 variables and 662 observations

# ... but one can see what variables are present:
description(nes1948)
#> 
#>  VVERSION 'NES VERSION NUMBER'        
#>  VDSETNO  'NES DATASET NUMBER'        
#>  V480001  'ICPSR ARCHIVE NUMBER'      
#>  V480002  'INTERVIEW NUMBER'          
#>  V480003  'POP CLASSIFICATION'        
#>  V480004  'CODER'                     
#>  V480005  'NUMBER OF CALLS TO R'      
#>  V480006  'R REMEMBER PREVIOUS INT'   
#>  V480007  'INTR INTERVIEW THIS R'     
#>  V480008  'PRVS PRE-ELCTN R REINT'    
#>  V480009  'R INT IN PRE/POSTELCTN'    
#>  V480010  'RENT CNTRL KEPT/DROPPED'   
#>  V480011  'GOVT CONTROL PRICES'       
#>  V480012  'WHAT TO DO W TFT-HT ACT'   
#>  V480013  'PRESLELCTN OTCM SURPRISE'  
#>  V480014A 'WHY PPL VTD FOR TRUMAN 1'  
#>  V480014B 'WHY PPL VTD FOR TRUMAN 2'  
#>  V480015A 'WHY PPL VTD AGNST TRUMAN 1'
#>  V480015B 'WHY PPL VTD AGNST TRUMAN 2'
#>  V480016A 'WHY PPL VTD FOR DEWEY 1'   
#>  V480016B 'WHY PPL VTD FOR DEWEY 2'   
#>  V480017A 'WHY PPL VTD AGNST DEWEY 1' 
#>  V480017B 'WHY PPL VTD AGNST DEWEY 2' 
#>  V480018  'DID R VOTE/FOR WHOM'       
#>  V480019  'WN DECIDE FOR WHOM TO VT'  
#>  V480020  'CNSD VT FOR SOMEONE ELSE'  
#>  V480021A 'XWHY DID NOT VT FOR HIM 1' 
#>  V480021B 'XWHY DID NOT VT FOR HIM 2' 
#>  V480022A 'WHY VT THE WAY YOU DID 1'  
#>  V480022B 'WHY VT THE WAY YOU DID 2'  
#>  V480023  'VOTED STRAIGHT TICKET'     
#>  V480024  'R NOT VT-IF VT,FOR WHOM'   
#>  V480025A 'R NOT VT-WHY DID NOT VT 1' 
#>  V480025B 'R NOT VT-WHY DID NOT VT 2' 
#>  V480026  'R NOT VT-WAS R REG TO VT'  
#>  V480027  'VTD IN PRVS PRESL ELCTN'   
#>  V480028  'VTD FOR WHOM IN 1944'      
#>  V480029  'OCCUPATION OF HEAD'        
#>  V480030  'HEAD BELONG TO LBR UN'     
#>  V480031A 'GRPS IDENTIFIED W TRUMAN 1'
#>  V480031B 'GRPS IDENTIFIED W TRUMAN 2'
#>  V480031C 'GRPS IDENTIFIED W TRUMAN 3'
#>  V480032A 'GRPS IDENTIFIED W DEWEY 1' 
#>  V480032B 'GRPS IDENTIFIED W DEWEY 2' 
#>  V480032C 'GRPS IDENTIFIED W DEWEY 3' 
#>  V480033A 'ISSUES CONNECTED W TRMN 1' 
#>  V480033B 'ISSUES CONNECTED W TRMN 2' 
#>  V480034A 'ISSUES CONNECTED W DEWEY 1'
#>  V480034B 'ISSUES CONNECTED W DEWEY 2'
#>  V480035A 'PERSONAL ATTRIBUTE TRMN 1' 
#>  V480035B 'PERSONAL ATTRIBUTE TRMN 2' 
#>  V480036A 'PERSONAL ATTRIBUTE DEWEY 1'
#>  V480036B 'PERSONAL ATTRIBUTE DEWEY 2'
#>  V480037  'CMPN INCIDENTS MENTIONED'  
#>  V480038  '41-PRESLELCTN PLAN TO VT'  
#>  V480039  '41-PLAN TO VT REP/DEM'     
#>  V480040  '41-USA'S CNCRN W OTHERS'   
#>  V480041  '41-SATISD USA TWRD RUSS'   
#>  V480042  '41-INFORMATION LEVEL'      
#>  V480043  '41-USA GV IN,AGRT RUSS'    
#>  V480044  '41-USA-RUSS AGRT VIA U.N'  
#>  V480045  'SEX OF RESPONDENT'         
#>  V480046  'RACE OF RESPONDENT'        
#>  V480047  'AGE OF RESPONDENT'         
#>  V480048  'EDUCATION OF RESPONDENT'   
#>  V480049  'TOTAL 1948 INCOME'         
#>  V480050  'RELIGIOUS PREFERENCE'      
#> 

# Now a subset of the data is loaded:
vote.socdem.48 <- subset(nes1948,
              select=c(
                  V480018,
                  V480029,
                  V480030,
                  V480045,
                  V480046,
                  V480047,
                  V480048,
                  V480049,
                  V480050
                  ))

# Let's make the names more descriptive:
vote.socdem.48 <- rename(vote.socdem.48,
                  V480018 = "vote",
                  V480029 = "occupation.hh",
                  V480030 = "unionized.hh",
                  V480045 = "gender",
                  V480046 = "race",
                  V480047 = "age",
                  V480048 = "education",
                  V480049 = "total.income",
                  V480050 = "religious.pref"
        )

# It is also possible to do both
# in one step:
# vote.socdem.48 <- subset(nes1948,
#              select=c(
#                  vote           = V480018,
#                  occupation.hh  = V480029,
#                  unionized.hh   = V480030,
#                  gender         = V480045,
#                  race           = V480046,
#                  age            = V480047,
#                  education      = V480048,
#                  total.income   = V480049,
#                  religious.pref = V480050
#                  ))



# We examine the data more closely:
codebook(vote.socdem.48)
#> ================================================================================
#> 
#>    vote 'DID R VOTE/FOR WHOM'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels             N Valid Total
#>                                               
#>    1   'VOTED - FOR TRUMAN'    212  32.1  32.0
#>    2   'VOTED - FOR DEWEY'     178  27.0  26.9
#>    3   'VOTED - FOR WALLACE'     1   0.2   0.2
#>    4   'VOTED - FOR OTHER'      11   1.7   1.7
#>    5   'VOTED - NA FOR WHOM'    20   3.0   3.0
#>    6   'DID NOT VOTE'          238  36.1  36.0
#>    9 M 'NA WHETHER VOTED'        2         0.3
#> 
#> ================================================================================
#> 
#>    occupation.hh 'OCCUPATION OF HEAD'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 99
#> 
#>    Values and labels                                  N Valid Total
#>                                                                    
#>    10   'PROFESSIONAL, SEMI-PROFESSIONAL'            44   6.9   6.6
#>    20   'SELF-EMPLOYED, MANAGERIAL, SUPERVISORY'     73  11.5  11.0
#>    30   'OTHER WHITE-COLLAR (CLERICAL, SALES, ET'    79  12.5  11.9
#>    40   'SKILLED AND SEMI-SKILLED'                  164  25.9  24.8
#>    60   'PROTECTIVE SERVICE'                          6   0.9   0.9
#>    70   'UNSKILLED, INCLUDING FARM AND SERVICE W'    85  13.4  12.8
#>    80   'FARM OPERATORS AND MANAGERS'               105  16.6  15.9
#>    92   'STUDENT'                                     7   1.1   1.1
#>    94   'UNEMPLOYED'                                  5   0.8   0.8
#>    95   'RETIRED, TOO OLD OR UNABLE TO WORK'         38   6.0   5.7
#>    96   'HOUSEWIFE'                                  28   4.4   4.2
#>    99 M 'NA'                                         28         4.2
#> 
#> ================================================================================
#> 
#>    unionized.hh 'HEAD BELONG TO LBR UN'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 8 - Inf
#> 
#>    Values and labels     N Valid Total
#>                                       
#>    1   'YES'           150  23.3  22.7
#>    2   'NO'            493  76.7  74.5
#>    8 M 'DK'              5         0.8
#>    9 M 'NA'             14         2.1
#> 
#> ================================================================================
#> 
#>    gender 'SEX OF RESPONDENT'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels     N Valid Total
#>                                       
#>    1   'MALE'          302  45.8  45.6
#>    2   'FEMALE'        357  54.2  53.9
#>    9 M 'NA'              3         0.5
#> 
#> ================================================================================
#> 
#>    race 'RACE OF RESPONDENT'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels     N Valid Total
#>                                       
#>    1   'WHITE'         585  90.7  88.4
#>    2   'NEGRO'          60   9.3   9.1
#>    3   'OTHER'           0   0.0   0.0
#>    9 M 'NA'             17         2.6
#> 
#> ================================================================================
#> 
#>    age 'AGE OF RESPONDENT'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels     N Valid Total
#>                                       
#>    1   '18-24'          57   8.7   8.6
#>    2   '25-34'         142  21.7  21.5
#>    3   '35-44'         174  26.6  26.3
#>    4   '45-54'         125  19.1  18.9
#>    5   '55-64'          86  13.1  13.0
#>    6   '65 AND OVER'    70  10.7  10.6
#>    9 M 'NA'              8         1.2
#> 
#> ================================================================================
#> 
#>    education 'EDUCATION OF RESPONDENT'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels      N Valid Total
#>                                        
#>    1   'GRADE SCHOOL'   292  44.4  44.1
#>    2   'HIGH SCHOOL'    266  40.4  40.2
#>    3   'COLLEGE'        100  15.2  15.1
#>    9 M 'NA'               4         0.6
#> 
#> ================================================================================
#> 
#>    total.income 'TOTAL 1948 INCOME'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels        N Valid Total
#>                                          
#>    1   'UNDER $500'        25   3.8   3.8
#>    2   '$500-$999'         43   6.6   6.5
#>    3   '$1000-1999'       110  16.8  16.6
#>    4   '$2000-2999'       185  28.2  27.9
#>    5   '$3000-3999'       142  21.7  21.5
#>    6   '$4000-4999'        66  10.1  10.0
#>    7   '$5000 AND OVER'    84  12.8  12.7
#>    9 M 'NA'                 7         1.1
#> 
#> ================================================================================
#> 
#>    religious.pref 'RELIGIOUS PREFERENCE'
#> 
#> --------------------------------------------------------------------------------
#> 
#>    Storage mode: double
#>    Measurement: nominal
#>    Missing values: 9
#> 
#>    Values and labels     N Valid Total
#>                                       
#>    1   'PROTESTANT'    460  70.0  69.5
#>    2   'CATHOLIC'      140  21.3  21.1
#>    3   'JEWISH'         25   3.8   3.8
#>    4   'OTHER'          14   2.1   2.1
#>    5   'NONE'           18   2.7   2.7
#>    9 M 'NA'              5         0.8
#> 

# ... and conduct some analyses.
#
t(genTable(percent(vote)~occupation.hh,data=vote.socdem.48))
#>                                          
#> occupation.hh                             VOTED - FOR TRUMAN VOTED - FOR DEWEY
#>   PROFESSIONAL, SEMI-PROFESSIONAL                  22.727273          50.00000
#>   SELF-EMPLOYED, MANAGERIAL, SUPERVISORY            9.589041          61.64384
#>   OTHER WHITE-COLLAR (CLERICAL, SALES, ET          37.974684          39.24051
#>   SKILLED AND SEMI-SKILLED                         51.829268          14.63415
#>   PROTECTIVE SERVICE                               16.666667          33.33333
#>   UNSKILLED, INCLUDING FARM AND SERVICE W          32.941176          11.76471
#>   FARM OPERATORS AND MANAGERS                      24.761905          13.33333
#>   STUDENT                                          14.285714          28.57143
#>   UNEMPLOYED                                        0.000000           0.00000
#>   RETIRED, TOO OLD OR UNABLE TO WORK               27.027027          43.24324
#>   HOUSEWIFE                                        17.857143          28.57143
#>   <NA>                                             33.333333          14.81481
#>                                          
#> occupation.hh                             VOTED - FOR WALLACE VOTED - FOR OTHER
#>   PROFESSIONAL, SEMI-PROFESSIONAL                   0.0000000          2.272727
#>   SELF-EMPLOYED, MANAGERIAL, SUPERVISORY            0.0000000          1.369863
#>   OTHER WHITE-COLLAR (CLERICAL, SALES, ET           0.0000000          0.000000
#>   SKILLED AND SEMI-SKILLED                          0.6097561          1.219512
#>   PROTECTIVE SERVICE                                0.0000000         16.666667
#>   UNSKILLED, INCLUDING FARM AND SERVICE W           0.0000000          0.000000
#>   FARM OPERATORS AND MANAGERS                       0.0000000          2.857143
#>   STUDENT                                           0.0000000          0.000000
#>   UNEMPLOYED                                        0.0000000          0.000000
#>   RETIRED, TOO OLD OR UNABLE TO WORK                0.0000000          2.702703
#>   HOUSEWIFE                                         0.0000000          0.000000
#>   <NA>                                              0.0000000          7.407407
#>                                          
#> occupation.hh                             VOTED - NA FOR WHOM DID NOT VOTE   N
#>   PROFESSIONAL, SEMI-PROFESSIONAL                    2.272727     22.72727  44
#>   SELF-EMPLOYED, MANAGERIAL, SUPERVISORY             1.369863     26.02740  73
#>   OTHER WHITE-COLLAR (CLERICAL, SALES, ET            5.063291     17.72152  79
#>   SKILLED AND SEMI-SKILLED                           2.439024     29.26829 164
#>   PROTECTIVE SERVICE                                 0.000000     33.33333   6
#>   UNSKILLED, INCLUDING FARM AND SERVICE W            4.705882     50.58824  85
#>   FARM OPERATORS AND MANAGERS                        1.904762     57.14286 105
#>   STUDENT                                            0.000000     57.14286   7
#>   UNEMPLOYED                                        20.000000     80.00000   5
#>   RETIRED, TOO OLD OR UNABLE TO WORK                 2.702703     24.32432  37
#>   HOUSEWIFE                                          0.000000     53.57143  28
#>   <NA>                                               7.407407     37.03704  27

# We consider only the two main candidates.
vote.socdem.48 <- within(vote.socdem.48,{
  truman.dewey <- vote
  valid.values(truman.dewey) <- 1:2
  truman.dewey <- relabel(truman.dewey,
              "VOTED - FOR TRUMAN" = "Truman",
              "VOTED - FOR DEWEY"  = "Dewey")
  })

summary(truman.relig.glm <- glm((truman.dewey=="Truman")~religious.pref,
    data=vote.socdem.48,
    family="binomial",
))
#> 
#> Call:
#> glm(formula = (truman.dewey == "Truman") ~ religious.pref, family = "binomial", 
#>     data = vote.socdem.48)
#> 
#> Coefficients:
#>                         Estimate Std. Error z value Pr(>|z|)   
#> (Intercept)             -0.13134    0.12831  -1.024  0.30604   
#> religious.prefCATHOLIC   0.79550    0.24442   3.255  0.00114 **
#> religious.prefJEWISH    16.69740  536.55453   0.031  0.97517   
#> religious.prefOTHER     -0.05099    0.61898  -0.082  0.93435   
#> religious.prefNONE      -0.20514    0.59943  -0.342  0.73219   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 537.69  on 389  degrees of freedom
#> Residual deviance: 500.69  on 385  degrees of freedom
#>   (272 observations deleted due to missingness)
#> AIC: 510.69
#> 
#> Number of Fisher Scoring iterations: 15
#> 
```
