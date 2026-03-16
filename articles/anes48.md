# Analysing the American National Election Study of 1948

## Introduction

This vignette gives an example for the analysis of a typical social
science data set. It is the data file of the *American National Election
Study* of 1948[¹](#fn1) available from the [American National Election
Studies website](https://electionstudies.org). The data file contains
data from to USA-wide surveys conducted October and November 1948 by the
Survey Research Centre, University Michigan (principal investigators:
Angus Campbell and Robert L. Kahn). The total number of cases in the
data set is 662 and the number of variables is 65 (more details about
this data set can be found at
<https://electionstudies.org/studypages/1948prepost/1948prepost.htm>).

With 662 cases and 65 variables, the 1948 ANES data set is relatively
small as compared to current social science data sets. Such larger data
sets can be processed along the same lines as in this vignette. Unlike
the 1948 ANES data, their size as well as, in some cases, legal
restrictions prohibit the inclusion of such a data set into the package,
however.

This vignette starts with a demonstration how a data file can be
examined before loading it and how a subset of the data can be loaded
into memory. After loading this subset into memory, some descriptive
analyses are conducted that showcase the construction of contingency
tables and of general tables of descriptive statistics using the
`genTable` function. In addition, a logit analysis is demonstrated and
the collection of several logit coefficients into a comprehensive table
by the `mtable` function.

It should be noted that the analyses reported in the following are
conducted only for purpose of demonstrating the features of the package
and are not to be considered of conclusive scientific evidence of any
kind.

This vignette is run with the help of the [*knitr*
package](https://cran.r-project.org/package=knitr). This allows to
showcase not only data management facilities provided by *memisc*. The
following code also demonstrates how output created with some of the
facilities of *memisc* can neatly integrated in reports generated with
*knitr*. Before we start, we adjust *knitr*’s output (with which this
vignette is formatted) to produce HTML where possible.

``` r
knit_print.codebook <-function(x,...) 
  knitr::asis_output(format_html(x,...))

knit_print.descriptions <-function(x,...) 
  knitr::asis_output(format_html(x,...))

knit_print.ftable <-function(x,options,...)
  knitr::asis_output(
    format_html(x,
                digits=if(length(options$ftable.digits))
                          options$ftable.digits
                       else 0,
                ...))
# We can now adjust the number of digits after the comma
# for each column e.g. by adding an `ftable.digits` option
# to an R chunk, as in ```{r,ftable=c(2,2,0)}

knit_print.mtable <-function(x,...)
  knitr::asis_output(format_html(x,...))
```

## Reading in a “portable” SPSS data file

We start with importing the data into R. The following code extracts the
SPSS portable file `NES1948.POR` from zip file `NES1948.ZIP` delivered
with the *memisc* package.

``` r
library(memisc)
options(digits=3)
nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())
```

Now the portable file is in a temporary directory and the path to the
file is contained in the string variable `nes1948.por`. In the next
step, the file is declared as a SPSS/PSPP portable file using the
function `spss.portable.file`, which as first argument takes the path to
the file. `spss.portable.file` reads in the information about the
variables contained in the data set and counts the number of cases in
the file. That is, standard I/O operations are used on the file, but the
data read in are just thrown away without allocating core memory for the
data. This counting of cases can, of course, be suppressed if it would
take too long.

``` r
nes1948 <- spss.portable.file(nes1948.por)
```

    Warning: 9 variables have duplicated labels:
      V480004, V480012, V480020, V480021A, V480021B, V480033A, V480033B,
      V480034A, V480034B

``` r
print(nes1948)
```

    SPSS portable file '/tmp/RtmpEr1txz/file230960359194/NES1948.POR' 
        with 67 variables and 662 observations

At this stage, the data are not loaded into the memory yet. But we can
see which variables exist inside the data set:

``` r
names(nes1948)
```

     [1] "VVERSION" "VDSETNO"  "V480001"  "V480002"  "V480003"  "V480004" 
     [7] "V480005"  "V480006"  "V480007"  "V480008"  "V480009"  "V480010" 
    [13] "V480011"  "V480012"  "V480013"  "V480014A" "V480014B" "V480015A"
    [19] "V480015B" "V480016A" "V480016B" "V480017A" "V480017B" "V480018" 
    [25] "V480019"  "V480020"  "V480021A" "V480021B" "V480022A" "V480022B"
    [31] "V480023"  "V480024"  "V480025A" "V480025B" "V480026"  "V480027" 
    [37] "V480028"  "V480029"  "V480030"  "V480031A" "V480031B" "V480031C"
    [43] "V480032A" "V480032B" "V480032C" "V480033A" "V480033B" "V480034A"
    [49] "V480034B" "V480035A" "V480035B" "V480036A" "V480036B" "V480037" 
    [55] "V480038"  "V480039"  "V480040"  "V480041"  "V480042"  "V480043" 
    [61] "V480044"  "V480045"  "V480046"  "V480047"  "V480048"  "V480049" 
    [67] "V480050" 

Note that the variable names are all changed from uppercase to lowercase
(SPSS does not distinguish uppercase and lowercase variable names and
uppercase looks like shouting). Casefolding could have been suppressed
by the call `spsp.portable.file(nes1948.por,tolower=FALSE)`.

We also can ask for a description (“variable label”) for each variable:

``` r
description(nes1948)
```

|          |                            |
|----------|----------------------------|
| Variable | Description                |
| VVERSION | NES VERSION NUMBER         |
| VDSETNO  | NES DATASET NUMBER         |
| V480001  | ICPSR ARCHIVE NUMBER       |
| V480002  | INTERVIEW NUMBER           |
| V480003  | POP CLASSIFICATION         |
| V480004  | CODER                      |
| V480005  | NUMBER OF CALLS TO R       |
| V480006  | R REMEMBER PREVIOUS INT    |
| V480007  | INTR INTERVIEW THIS R      |
| V480008  | PRVS PRE-ELCTN R REINT     |
| V480009  | R INT IN PRE/POSTELCTN     |
| V480010  | RENT CNTRL KEPT/DROPPED    |
| V480011  | GOVT CONTROL PRICES        |
| V480012  | WHAT TO DO W TFT-HT ACT    |
| V480013  | PRESLELCTN OTCM SURPRISE   |
| V480014A | WHY PPL VTD FOR TRUMAN 1   |
| V480014B | WHY PPL VTD FOR TRUMAN 2   |
| V480015A | WHY PPL VTD AGNST TRUMAN 1 |
| V480015B | WHY PPL VTD AGNST TRUMAN 2 |
| V480016A | WHY PPL VTD FOR DEWEY 1    |
| V480016B | WHY PPL VTD FOR DEWEY 2    |
| V480017A | WHY PPL VTD AGNST DEWEY 1  |
| V480017B | WHY PPL VTD AGNST DEWEY 2  |
| V480018  | DID R VOTE/FOR WHOM        |
| V480019  | WN DECIDE FOR WHOM TO VT   |
| V480020  | CNSD VT FOR SOMEONE ELSE   |
| V480021A | XWHY DID NOT VT FOR HIM 1  |
| V480021B | XWHY DID NOT VT FOR HIM 2  |
| V480022A | WHY VT THE WAY YOU DID 1   |
| V480022B | WHY VT THE WAY YOU DID 2   |
| V480023  | VOTED STRAIGHT TICKET      |
| V480024  | R NOT VT-IF VT,FOR WHOM    |
| V480025A | R NOT VT-WHY DID NOT VT 1  |
| V480025B | R NOT VT-WHY DID NOT VT 2  |
| V480026  | R NOT VT-WAS R REG TO VT   |
| V480027  | VTD IN PRVS PRESL ELCTN    |
| V480028  | VTD FOR WHOM IN 1944       |
| V480029  | OCCUPATION OF HEAD         |
| V480030  | HEAD BELONG TO LBR UN      |
| V480031A | GRPS IDENTIFIED W TRUMAN 1 |
| V480031B | GRPS IDENTIFIED W TRUMAN 2 |
| V480031C | GRPS IDENTIFIED W TRUMAN 3 |
| V480032A | GRPS IDENTIFIED W DEWEY 1  |
| V480032B | GRPS IDENTIFIED W DEWEY 2  |
| V480032C | GRPS IDENTIFIED W DEWEY 3  |
| V480033A | ISSUES CONNECTED W TRMN 1  |
| V480033B | ISSUES CONNECTED W TRMN 2  |
| V480034A | ISSUES CONNECTED W DEWEY 1 |
| V480034B | ISSUES CONNECTED W DEWEY 2 |
| V480035A | PERSONAL ATTRIBUTE TRMN 1  |
| V480035B | PERSONAL ATTRIBUTE TRMN 2  |
| V480036A | PERSONAL ATTRIBUTE DEWEY 1 |
| V480036B | PERSONAL ATTRIBUTE DEWEY 2 |
| V480037  | CMPN INCIDENTS MENTIONED   |
| V480038  | 41-PRESLELCTN PLAN TO VT   |
| V480039  | 41-PLAN TO VT REP/DEM      |
| V480040  | 41-USA’S CNCRN W OTHERS    |
| V480041  | 41-SATISD USA TWRD RUSS    |
| V480042  | 41-INFORMATION LEVEL       |
| V480043  | 41-USA GV IN,AGRT RUSS     |
| V480044  | 41-USA-RUSS AGRT VIA U.N   |
| V480045  | SEX OF RESPONDENT          |
| V480046  | RACE OF RESPONDENT         |
| V480047  | AGE OF RESPONDENT          |
| V480048  | EDUCATION OF RESPONDENT    |
| V480049  | TOTAL 1948 INCOME          |
| V480050  | RELIGIOUS PREFERENCE       |

or even a code book using

``` r
codebook(nes1948)
```

(this is not shown here because the output would have taken more then
thirty pages). We can also get a codebook of the first few variabels
instead, with

``` r
codebook(nes1948[1:5])
```

`VVERSION` — ‘NES VERSION NUMBER’

|               |          |
|---------------|----------|
| Storage mode: | double   |
| Measurement:  | interval |

|           |     |     |     |
|-----------|-----|-----|-----|
| Min:      | 1   | .   | 000 |
| Max:      | 1   | .   | 000 |
| Mean:     | 1   | .   | 000 |
| Std.Dev.: | 0   | .   | 000 |

`VDSETNO` — ‘NES DATASET NUMBER’

|               |           |
|---------------|-----------|
| Storage mode: | character |
| Measurement:  | nominal   |

|      |       |     |     |
|------|-------|-----|-----|
| Min: | “1948 |     | T”  |
| Max: | “1948 |     | T”  |

`V480001` — ‘ICPSR ARCHIVE NUMBER’

|               |          |
|---------------|----------|
| Storage mode: | double   |
| Measurement:  | interval |

|           |      |     |     |
|-----------|------|-----|-----|
| Min:      | 7218 | .   | 000 |
| Max:      | 7218 | .   | 000 |
| Mean:     | 7218 | .   | 000 |
| Std.Dev.: | 0    | .   | 000 |

`V480002` — ‘INTERVIEW NUMBER’

|               |          |
|---------------|----------|
| Storage mode: | double   |
| Measurement:  | interval |

|           |      |     |     |
|-----------|------|-----|-----|
| Min:      | 1001 | .   | 000 |
| Max:      | 1662 | .   | 000 |
| Mean:     | 1331 | .   | 500 |
| Std.Dev.: | 191  | .   | 103 |

`V480003` — ‘POP CLASSIFICATION’

|               |         |
|---------------|---------|
| Storage mode: | double  |
| Measurement:  | nominal |

|                   |     |                     |     |         |     |     |
|-------------------|-----|---------------------|-----|---------|-----|-----|
| Values and labels |     |                     | N   | Percent |     |     |
| 1                 |     | ‘METROPOLITAN AREA’ | 182 | 27      | .   | 5   |
| 2                 |     | ‘TOWN OR CITY’      | 354 | 53      | .   | 5   |
| 3                 |     | ‘OPEN COUNTRY’      | 126 | 19      | .   | 0   |

#### Reading in a subset of the data

After we have decided which variables to use we can read in a subset of
the data:

``` r
vote.48 <- subset(nes1948,
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
```

The subset of the ANES 1948 we read in is now contained in the variable
`vote.48`, which contains an object of class `data.set`. A `data.set` is
an “embellished” version of a `data.frame`, a data structure intended to
contained `labelled` vectors. `labelled` vectors contain the all the
special information attached to the variables in the original data set,
such as variable labels, value labels, and general missing values. A
short summary of this special information shows up after a call to
`str`.

``` r
str(vote.48)
```

    Data set with 662 obs. of 9 variables:
     $ V480018: Nmnl. item w/ 7 labels for 1,2,3,... + ms.v.  num  1 2 1 2 1 2 2 1 2 1 ...
     $ V480029: Nmnl. item w/ 12 labels for 10,20,30,... + ms.v.  num  70 30 40 10 10 20 80 80 40 40 ...
     $ V480030: Nmnl. item w/ 4 labels for 1,2,8,... + ms.v.  num  1 2 2 2 2 2 2 2 1 1 ...
     $ V480045: Nmnl. item w/ 3 labels for 1,2,9 + ms.v.  num  1 2 2 2 1 2 1 2 1 1 ...
     $ V480046: Nmnl. item w/ 4 labels for 1,2,3,... + ms.v.  num  1 1 1 1 1 1 1 1 1 1 ...
     $ V480047: Nmnl. item w/ 7 labels for 1,2,3,... + ms.v.  num  3 3 2 3 2 3 4 5 2 2 ...
     $ V480048: Nmnl. item w/ 4 labels for 1,2,3,... + ms.v.  num  1 2 2 3 3 2 1 1 2 2 ...
     $ V480049: Nmnl. item w/ 8 labels for 1,2,3,... + ms.v.  num  4 7 5 7 5 7 5 2 5 6 ...
     $ V480050: Nmnl. item w/ 6 labels for 1,2,3,... + ms.v.  num  1 1 2 1 2 1 1 1 1 2 ...

This output shows, for example, that variable `V480018` has the
description (variable label) “DID R VOTE/FOR WHOM” is considered as
having nominal level of measurement, has seven value labels and one
defined missing value.

Since the variable names in the ANES data set are not very mnemonic, we
rename the variables:

``` r
vote.48 <- rename(vote.48,
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
```

Since many data sets available from public repositories have such
non-mnemonic variable names as in this example, it might be convenient
to do the data loading and renaming in one step. Indeed it is possible:

``` r
vote.48 <- subset(nes1948,
                  select=c(
                    vote           = V480018,
                    occupation.hh  = V480029,
                    unionized.hh   = V480030,
                    gender         = V480045,
                    race           = V480046,
                    age            = V480047,
                    education      = V480048,
                    total.income   = V480049,
                    religious.pref = V480050
                  ))
```

Before we start with analyses, we take a closer look at the data.

``` r
codebook(vote.48)
```

`vote` — ‘DID R VOTE/FOR WHOM’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |                       |     |       |     |     |       |     |     |
|-------------------|-----|-----------------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                       | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘VOTED - FOR TRUMAN’  | 212 | 32    | .   | 1   | 32    | .   | 0   |
| 2                 |     | ‘VOTED - FOR DEWEY’   | 178 | 27    | .   | 0   | 26    | .   | 9   |
| 3                 |     | ‘VOTED - FOR WALLACE’ | 1   | 0     | .   | 2   | 0     | .   | 2   |
| 4                 |     | ‘VOTED - FOR OTHER’   | 11  | 1     | .   | 7   | 1     | .   | 7   |
| 5                 |     | ‘VOTED - NA FOR WHOM’ | 20  | 3     | .   | 0   | 3     | .   | 0   |
| 6                 |     | ‘DID NOT VOTE’        | 238 | 36    | .   | 1   | 36    | .   | 0   |
| 9                 | M   | ‘NA WHETHER VOTED’    | 2   |       |     |     | 0     | .   | 3   |

`occupation.hh` — ‘OCCUPATION OF HEAD’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 99      |

|                   |     |                                           |     |       |     |     |       |     |     |
|-------------------|-----|-------------------------------------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                                           | N   | Valid |     |     | Total |     |     |
| 10                |     | ‘PROFESSIONAL, SEMI-PROFESSIONAL’         | 44  | 6     | .   | 9   | 6     | .   | 6   |
| 20                |     | ‘SELF-EMPLOYED, MANAGERIAL, SUPERVISORY’  | 73  | 11    | .   | 5   | 11    | .   | 0   |
| 30                |     | ‘OTHER WHITE-COLLAR (CLERICAL, SALES, ET’ | 79  | 12    | .   | 5   | 11    | .   | 9   |
| 40                |     | ‘SKILLED AND SEMI-SKILLED’                | 164 | 25    | .   | 9   | 24    | .   | 8   |
| 60                |     | ‘PROTECTIVE SERVICE’                      | 6   | 0     | .   | 9   | 0     | .   | 9   |
| 70                |     | ‘UNSKILLED, INCLUDING FARM AND SERVICE W’ | 85  | 13    | .   | 4   | 12    | .   | 8   |
| 80                |     | ‘FARM OPERATORS AND MANAGERS’             | 105 | 16    | .   | 6   | 15    | .   | 9   |
| 92                |     | ‘STUDENT’                                 | 7   | 1     | .   | 1   | 1     | .   | 1   |
| 94                |     | ‘UNEMPLOYED’                              | 5   | 0     | .   | 8   | 0     | .   | 8   |
| 95                |     | ‘RETIRED, TOO OLD OR UNABLE TO WORK’      | 38  | 6     | .   | 0   | 5     | .   | 7   |
| 96                |     | ‘HOUSEWIFE’                               | 28  | 4     | .   | 4   | 4     | .   | 2   |
| 99                | M   | ‘NA’                                      | 28  |       |     |     | 4     | .   | 2   |

`unionized.hh` — ‘HEAD BELONG TO LBR UN’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 8 - Inf |

|                   |     |       |     |       |     |     |       |     |     |
|-------------------|-----|-------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |       | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘YES’ | 150 | 23    | .   | 3   | 22    | .   | 7   |
| 2                 |     | ‘NO’  | 493 | 76    | .   | 7   | 74    | .   | 5   |
| 8                 | M   | ‘DK’  | 5   |       |     |     | 0     | .   | 8   |
| 9                 | M   | ‘NA’  | 14  |       |     |     | 2     | .   | 1   |

`gender` — ‘SEX OF RESPONDENT’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |          |     |       |     |     |       |     |     |
|-------------------|-----|----------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |          | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘MALE’   | 302 | 45    | .   | 8   | 45    | .   | 6   |
| 2                 |     | ‘FEMALE’ | 357 | 54    | .   | 2   | 53    | .   | 9   |
| 9                 | M   | ‘NA’     | 3   |       |     |     | 0     | .   | 5   |

`race` — ‘RACE OF RESPONDENT’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |         |     |       |     |     |       |     |     |
|-------------------|-----|---------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |         | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘WHITE’ | 585 | 90    | .   | 7   | 88    | .   | 4   |
| 2                 |     | ‘NEGRO’ | 60  | 9     | .   | 3   | 9     | .   | 1   |
| 3                 |     | ‘OTHER’ | 0   | 0     | .   | 0   | 0     | .   | 0   |
| 9                 | M   | ‘NA’    | 17  |       |     |     | 2     | .   | 6   |

`age` — ‘AGE OF RESPONDENT’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |               |     |       |     |     |       |     |     |
|-------------------|-----|---------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |               | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘18-24’       | 57  | 8     | .   | 7   | 8     | .   | 6   |
| 2                 |     | ‘25-34’       | 142 | 21    | .   | 7   | 21    | .   | 5   |
| 3                 |     | ‘35-44’       | 174 | 26    | .   | 6   | 26    | .   | 3   |
| 4                 |     | ‘45-54’       | 125 | 19    | .   | 1   | 18    | .   | 9   |
| 5                 |     | ‘55-64’       | 86  | 13    | .   | 1   | 13    | .   | 0   |
| 6                 |     | ‘65 AND OVER’ | 70  | 10    | .   | 7   | 10    | .   | 6   |
| 9                 | M   | ‘NA’          | 8   |       |     |     | 1     | .   | 2   |

`education` — ‘EDUCATION OF RESPONDENT’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |                |     |       |     |     |       |     |     |
|-------------------|-----|----------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘GRADE SCHOOL’ | 292 | 44    | .   | 4   | 44    | .   | 1   |
| 2                 |     | ‘HIGH SCHOOL’  | 266 | 40    | .   | 4   | 40    | .   | 2   |
| 3                 |     | ‘COLLEGE’      | 100 | 15    | .   | 2   | 15    | .   | 1   |
| 9                 | M   | ‘NA’           | 4   |       |     |     | 0     | .   | 6   |

`total.income` — ‘TOTAL 1948 INCOME’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |                   |     |       |     |     |       |     |     |
|-------------------|-----|-------------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |                   | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘UNDER \$500’     | 25  | 3     | .   | 8   | 3     | .   | 8   |
| 2                 |     | ‘\$500-\$999’     | 43  | 6     | .   | 6   | 6     | .   | 5   |
| 3                 |     | ‘\$1000-1999’     | 110 | 16    | .   | 8   | 16    | .   | 6   |
| 4                 |     | ‘\$2000-2999’     | 185 | 28    | .   | 2   | 27    | .   | 9   |
| 5                 |     | ‘\$3000-3999’     | 142 | 21    | .   | 7   | 21    | .   | 5   |
| 6                 |     | ‘\$4000-4999’     | 66  | 10    | .   | 1   | 10    | .   | 0   |
| 7                 |     | ‘\$5000 AND OVER’ | 84  | 12    | .   | 8   | 12    | .   | 7   |
| 9                 | M   | ‘NA’              | 7   |       |     |     | 1     | .   | 1   |

`religious.pref` — ‘RELIGIOUS PREFERENCE’

|                 |         |
|-----------------|---------|
| Storage mode:   | double  |
| Measurement:    | nominal |
| Missing values: | 9       |

|                   |     |              |     |       |     |     |       |     |     |
|-------------------|-----|--------------|-----|-------|-----|-----|-------|-----|-----|
| Values and labels |     |              | N   | Valid |     |     | Total |     |     |
| 1                 |     | ‘PROTESTANT’ | 460 | 70    | .   | 0   | 69    | .   | 5   |
| 2                 |     | ‘CATHOLIC’   | 140 | 21    | .   | 3   | 21    | .   | 1   |
| 3                 |     | ‘JEWISH’     | 25  | 3     | .   | 8   | 3     | .   | 8   |
| 4                 |     | ‘OTHER’      | 14  | 2     | .   | 1   | 2     | .   | 1   |
| 5                 |     | ‘NONE’       | 18  | 2     | .   | 7   | 2     | .   | 7   |
| 9                 | M   | ‘NA’         | 5   |       |     |     | 0     | .   | 8   |

We now have obtained a *codebook*, which contains information of the
class and type of the variables in the data set, the value labels and
defined missing values, and counts of the distinct values of the
variables.

## Analysis

### Some descriptive analyses

We start our analyses with a contingency table, but first we make some
preparations: We recode the variables of interest into a smaller number
of categories in order to get results that are easier to read and
interpret.

``` r
vote.48 <- within(vote.48,{
  vote3 <- recode(vote,
    1 -> "Truman",
    2 -> "Dewey",
    3:4 -> "Other"
    )
  occup4 <- recode(occupation.hh,
    10:20 -> "Upper white collar",
    30 -> "Other white collar",
    40:70 -> "Blue collar",
    80 -> "Farmer"
    )
  relig3 <- recode(religious.pref,
    1 -> "Protestant",
    2 -> "Catholic",
    3:5 -> "Other,none"
    )
   race2 <- recode(race,
    1 -> "White",
    2 -> "Black"
    )
  })
```

    Warning in recode(vote, "Truman" <- 1, "Dewey" <- 2, "Other" <- 3:4): recoding
    created 260 NAs

    Warning in recode(occupation.hh, "Upper white collar" <- 10:20, "Other white
    collar" <- 30, : recoding created 106 NAs

    Warning in recode(religious.pref, "Protestant" <- 1, "Catholic" <- 2,
    "Other,none" <- 3:5): recoding created 5 NAs

    Warning in recode(race, "White" <- 1, "Black" <- 2): recoding created 17 NAs

Having constructed the unordered factors `vote3`, `occup4`, `relig3`,
and `race2` we can proceed examining the association the vote,
occupational class, relgious denomination, and race. First, we look upon
a simple contingency table.

``` r
ftable(xtabs(~vote3+occup4,data=vote.48))
```

|        |     |                    |     |     |                    |     |     |             |     |     |        |     |     |
|--------|-----|--------------------|-----|-----|--------------------|-----|-----|-------------|-----|-----|--------|-----|-----|
|        |     | occup4             |     |     |                    |     |     |             |     |     |        |     |     |
| vote3  |     | Upper white collar |     |     | Other white collar |     |     | Blue collar |     |     | Farmer |     |     |
| Truman |     | 17                 |     |     | 30                 |     |     | 114         |     |     | 26     |     |     |
| Dewey  |     | 67                 |     |     | 31                 |     |     | 36          |     |     | 14     |     |     |
| Other  |     | 2                  |     |     | 0                  |     |     | 4           |     |     | 3      |     |     |

Tables of percentages may seem more informative about the impact of
various factors on the vote. So we use the function `genTable` to obtain
such tables of percentages:

``` r
gt1 <- genTable(percent(vote3)~occup4,data=vote.48)
## For knitr-ing, we use ```{r, ftable.digits=c(2,2,2,0)} here.
ftable(gt1,row.vars=2)
```

|                    |     |        |     |     |       |     |     |       |     |     |     |     |     |
|--------------------|-----|--------|-----|-----|-------|-----|-----|-------|-----|-----|-----|-----|-----|
| occup4             |     | Truman |     |     | Dewey |     |     | Other |     |     | N   |     |     |
| Upper white collar |     | 19     | .   | 77  | 77    | .   | 91  | 2     | .   | 33  | 86  |     |     |
| Other white collar |     | 49     | .   | 18  | 50    | .   | 82  | 0     | .   | 00  | 61  |     |     |
| Blue collar        |     | 74     | .   | 03  | 23    | .   | 38  | 2     | .   | 60  | 154 |     |     |
| Farmer             |     | 60     | .   | 47  | 32    | .   | 56  | 6     | .   | 98  | 43  |     |     |
| NA                 |     | 43     | .   | 10  | 51    | .   | 72  | 5     | .   | 17  | 58  |     |     |

Obviously, voters from farmer and blue collar worker households were
especially supportive of President Truman, while voters of upper white
collar background largely supported the Republican Candidate Dewey.

``` r
gt2 <- genTable(percent(vote3)~relig3,data=vote.48)
ftable(gt2,row.vars=2)
```

|            |     |        |     |     |       |     |     |       |     |     |     |     |     |
|------------|-----|--------|-----|-----|-------|-----|-----|-------|-----|-----|-----|-----|-----|
| relig3     |     | Truman |     |     | Dewey |     |     | Other |     |     | N   |     |     |
| Protestant |     | 44     | .   | 71  | 50    | .   | 98  | 4     | .   | 31  | 255 |     |     |
| Catholic   |     | 66     | .   | 02  | 33    | .   | 98  | 0     | .   | 00  | 103 |     |     |
| Other,none |     | 68     | .   | 18  | 29    | .   | 55  | 2     | .   | 27  | 44  |     |     |
| NA         |     | NaN    |     |     | NaN   |     |     | NaN   |     |     | 0   |     |     |

This table shows that Catholics and adherents of other denominations
were more supportive of Truman than of Dewey.

``` r
gt3 <- genTable(percent(vote3)~race2,data=vote.48)
ftable(gt3,row.vars=2)
```

|       |     |        |     |     |       |     |     |       |     |     |     |     |     |
|-------|-----|--------|-----|-----|-------|-----|-----|-------|-----|-----|-----|-----|-----|
| race2 |     | Truman |     |     | Dewey |     |     | Other |     |     | N   |     |     |
| White |     | 51     | .   | 33  | 45    | .   | 48  | 3     | .   | 19  | 376 |     |     |
| Black |     | 64     | .   | 71  | 35    | .   | 29  | 0     | .   | 00  | 17  |     |     |
| NA    |     | 88     | .   | 89  | 11    | .   | 11  | 0     | .   | 00  | 9   |     |     |

African Americans apparently supported Truman by a large majority. The
number of members of this group in the sample is very small, however, so
that such an inference would be very shaky.

``` r
gt4 <- genTable(percent(vote3)~total.income,data=vote.48)
ftable(gt4,row.vars=2)
```

|                 |     |        |     |     |       |     |     |       |     |     |     |     |     |
|-----------------|-----|--------|-----|-----|-------|-----|-----|-------|-----|-----|-----|-----|-----|
| total.income    |     | Truman |     |     | Dewey |     |     | Other |     |     | N   |     |     |
| UNDER \$500     |     | 50     | .   | 00  | 50    | .   | 00  | 0     | .   | 00  | 8   |     |     |
| \$500-\$999     |     | 61     | .   | 54  | 38    | .   | 46  | 0     | .   | 00  | 13  |     |     |
| \$1000-1999     |     | 64     | .   | 41  | 32    | .   | 20  | 3     | .   | 39  | 59  |     |     |
| \$2000-2999     |     | 66     | .   | 99  | 30    | .   | 10  | 2     | .   | 91  | 103 |     |     |
| \$3000-3999     |     | 47     | .   | 52  | 48    | .   | 51  | 3     | .   | 96  | 101 |     |     |
| \$4000-4999     |     | 45     | .   | 83  | 50    | .   | 00  | 4     | .   | 17  | 48  |     |     |
| \$5000 AND OVER |     | 31     | .   | 82  | 68    | .   | 18  | 0     | .   | 00  | 66  |     |     |
| NA              |     | 50     | .   | 00  | 25    | .   | 00  | 25    | .   | 00  | 4   |     |     |

The table of percentage of vote by income suggests that income had some
considerable influence on the choice either of Truman or of Dewey, but
the unequal distribution of income categories warrants a more refined
analysis that takes into account the uncertainty about the vote
percentages. Therefore, the percentages of support for Truman broken
down by income shown with confidence intervals:

``` r
## For knitr-ing, we use ```{r, ftable.digits=c(2,2,2)} here.
inc.tab <- genTable(percent(vote3,ci=TRUE)~total.income,data=vote.48)
ftable(inc.tab,row.vars=c(3,2))
```

|                 |        |     |            |     |     |       |     |     |       |     |     |
|-----------------|--------|-----|------------|-----|-----|-------|-----|-----|-------|-----|-----|
| total.income    | vote3  |     | Percentage |     |     | lower |     |     | upper |     |     |
| UNDER \$500     | Truman |     | 50         | .   | 00  | 15    | .   | 70  | 84    | .   | 30  |
|                 | Dewey  |     | 50         | .   | 00  | 15    | .   | 70  | 84    | .   | 30  |
|                 | Other  |     | 0          | .   | 00  | 0     | .   | 00  | 36    | .   | 94  |
| \$500-\$999     | Truman |     | 61         | .   | 54  | 31    | .   | 58  | 86    | .   | 14  |
|                 | Dewey  |     | 38         | .   | 46  | 13    | .   | 86  | 68    | .   | 42  |
|                 | Other  |     | 0          | .   | 00  | 0     | .   | 00  | 24    | .   | 71  |
| \$1000-1999     | Truman |     | 64         | .   | 41  | 50    | .   | 87  | 76    | .   | 45  |
|                 | Dewey  |     | 32         | .   | 20  | 20    | .   | 62  | 45    | .   | 64  |
|                 | Other  |     | 3          | .   | 39  | 0     | .   | 41  | 11    | .   | 71  |
| \$2000-2999     | Truman |     | 66         | .   | 99  | 57    | .   | 03  | 75    | .   | 94  |
|                 | Dewey  |     | 30         | .   | 10  | 21    | .   | 45  | 39    | .   | 92  |
|                 | Other  |     | 2          | .   | 91  | 0     | .   | 60  | 8     | .   | 28  |
| \$3000-3999     | Truman |     | 47         | .   | 52  | 37    | .   | 49  | 57    | .   | 70  |
|                 | Dewey  |     | 48         | .   | 51  | 38    | .   | 45  | 58    | .   | 67  |
|                 | Other  |     | 3          | .   | 96  | 1     | .   | 09  | 9     | .   | 83  |
| \$4000-4999     | Truman |     | 45         | .   | 83  | 31    | .   | 37  | 60    | .   | 83  |
|                 | Dewey  |     | 50         | .   | 00  | 35    | .   | 23  | 64    | .   | 77  |
|                 | Other  |     | 4          | .   | 17  | 0     | .   | 51  | 14    | .   | 25  |
| \$5000 AND OVER | Truman |     | 31         | .   | 82  | 20    | .   | 89  | 44    | .   | 44  |
|                 | Dewey  |     | 68         | .   | 18  | 55    | .   | 56  | 79    | .   | 11  |
|                 | Other  |     | 0          | .   | 00  | 0     | .   | 00  | 5     | .   | 44  |
| NA              | Truman |     | 50         | .   | 00  | 6     | .   | 76  | 93    | .   | 24  |
|                 | Dewey  |     | 25         | .   | 00  | 0     | .   | 63  | 80    | .   | 59  |
|                 | Other  |     | 25         | .   | 00  | 0     | .   | 63  | 80    | .   | 59  |

Occupational class is more evenly distributed in the sample, thus it may
be possible to obtain more precise estimates of the percentages of
support for Truman for occupational classes:

``` r
occup.tab <- genTable(percent(vote3,ci=TRUE)~occup4,data=vote.48)
ftable(occup.tab,row.vars=c(3,2))
```

|                    |        |     |            |     |     |       |     |     |       |     |     |
|--------------------|--------|-----|------------|-----|-----|-------|-----|-----|-------|-----|-----|
| occup4             | vote3  |     | Percentage |     |     | lower |     |     | upper |     |     |
| Upper white collar | Truman |     | 19         | .   | 77  | 11    | .   | 96  | 29    | .   | 75  |
|                    | Dewey  |     | 77         | .   | 91  | 67    | .   | 67  | 86    | .   | 14  |
|                    | Other  |     | 2          | .   | 33  | 0     | .   | 28  | 8     | .   | 15  |
| Other white collar | Truman |     | 49         | .   | 18  | 36    | .   | 14  | 62    | .   | 30  |
|                    | Dewey  |     | 50         | .   | 82  | 37    | .   | 70  | 63    | .   | 86  |
|                    | Other  |     | 0          | .   | 00  | 0     | .   | 00  | 5     | .   | 87  |
| Blue collar        | Truman |     | 74         | .   | 03  | 66    | .   | 35  | 80    | .   | 75  |
|                    | Dewey  |     | 23         | .   | 38  | 16    | .   | 94  | 30    | .   | 86  |
|                    | Other  |     | 2          | .   | 60  | 0     | .   | 71  | 6     | .   | 52  |
| Farmer             | Truman |     | 60         | .   | 47  | 44    | .   | 41  | 75    | .   | 02  |
|                    | Dewey  |     | 32         | .   | 56  | 19    | .   | 08  | 48    | .   | 54  |
|                    | Other  |     | 6          | .   | 98  | 1     | .   | 46  | 19    | .   | 06  |
| NA                 | Truman |     | 43         | .   | 10  | 30    | .   | 16  | 56    | .   | 77  |
|                    | Dewey  |     | 51         | .   | 72  | 38    | .   | 22  | 65    | .   | 05  |
|                    | Other  |     | 5          | .   | 17  | 1     | .   | 08  | 14    | .   | 38  |

The upper and lower white-collar and blue-collar classes are quite
distinct with regard to the percentages of support for Truman. The point
estimates of the percentages are outside the confidence intervals of the
respective other occupational classes, the confidence intervals do not
even overlap. However, it is not clear whether farmers are distinct from
the blue-collar and lower white-collar classes.

#### Logit modelling of candidate choice

In the following we conduct a logit analysis of the vote for Truman.
First, we assign non-standard contrasts the categorical predictors.
Here, the function `contr` is used to assign treatment (dummy) contrasts
to `occup4` and `total.income` with baseline category 3 and 4,
respectively.

``` r
vote.48 <- within(vote.48,{
  contrasts(occup4) <- contr("treatment",base = 3)
  contrasts(total.income) <- contr("treatment",base = 4)
  })
```

We now fit some logistic regression models of the impact occupational
class, income, and religious denomination on the vote choice supporting
Truman. The contrasts of the occupational class and income factors are
such that they compare the choices of the members of the blue-collar
class with all other classes and the middle income group (\$ 2000-2999)
with the other income groups. The religious denomination factor compares
Protestants with Catholics and those with other or no denominations.

``` r
model1 <- glm((vote3=="Truman")~occup4,data=vote.48,
              family="binomial")
model2 <- glm((vote3=="Truman")~total.income,data=vote.48,
              family="binomial")
model3 <- glm((vote3=="Truman")~occup4+total.income,data=vote.48,
              family="binomial")
model4 <- glm((vote3=="Truman")~relig3,data=vote.48,
              family="binomial")
model5 <- glm((vote3=="Truman")~occup4+relig3,data=vote.48,
              family="binomial")
```

First, we use `mtable` to construct a comparative table of the estimates
of `model1`, `model2`, and `model3`. We thus can compare the impact of
occupational class and income on the choice of candidate Truman.

``` r
mtable(model1,model2,model3,summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N"))
```

    Calls:
    model1: glm(formula = (vote3 == "Truman") ~ occup4, family = "binomial", 
        data = vote.48)
    model2: glm(formula = (vote3 == "Truman") ~ total.income, family = "binomial", 
        data = vote.48)
    model3: glm(formula = (vote3 == "Truman") ~ occup4 + total.income, family = "binomial", 
        data = vote.48)

    ===============================================================================
                                                 model1      model2      model3    
    -------------------------------------------------------------------------------
      (Intercept)                                1.047***    0.708***    1.316***  
                                                (0.184)     (0.210)     (0.268)    
      occup4: Upper white collar/Blue collar    -2.448***               -2.328***  
                                                (0.327)                 (0.357)    
      occup4: Other white collar/Blue collar    -1.080***               -1.015**   
                                                (0.315)                 (0.323)    
      occup4: Farmer/Blue collar                -0.622                  -0.792*    
                                                (0.362)                 (0.383)    
      total.income: UNDER $500/$2000-2999                   -0.708      -0.662     
                                                            (0.737)     (1.056)    
      total.income: $500-$999/$2000-2999                    -0.238       0.912     
                                                            (0.607)     (1.143)    
      total.income: $1000-1999/$2000-2999                   -0.115       0.144     
                                                            (0.343)     (0.440)    
      total.income: $3000-3999/$2000-2999                   -0.807**    -0.527     
                                                            (0.289)     (0.338)    
      total.income: $4000-4999/$2000-2999                   -0.875*     -0.509     
                                                            (0.358)     (0.411)    
      total.income: $5000 AND OVER/$2000-2999               -1.470***   -0.535     
                                                            (0.337)     (0.405)    
    -------------------------------------------------------------------------------
      Nagelkerke R-sq.                           0.246       0.085       0.274     
      Deviance                                 404.190     524.433     390.551     
      AIC                                      412.190     538.433     410.551     
      N                                        344         398         340         
    ===============================================================================
      Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  

`mtable` returns an object of class `"mtable"`. When formatted it looks
close to the requirements of typical social science publications. Yet at
least we want to change the technical variable names into non-technical
ones, for which we can use `relabel`:

``` r
relabel(mtable(
            "Model 1"=model1,
            "Model 2"=model2,
            "Model 3"=model3,
            summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N")),
          UNDER="under",
          "AND OVER"="and over",
          occup4="Occup. class",
          total.income="Income",
          gsub=TRUE
          )
```

    Calls:
    Model 1: glm(formula = (vote3 == "Truman") ~ occup4, family = "binomial", 
        data = vote.48)
    Model 2: glm(formula = (vote3 == "Truman") ~ total.income, family = "binomial", 
        data = vote.48)
    Model 3: glm(formula = (vote3 == "Truman") ~ occup4 + total.income, family = "binomial", 
        data = vote.48)

    ====================================================================================
                                                     Model 1     Model 2     Model 3    
    ------------------------------------------------------------------------------------
      (Intercept)                                     1.047***    0.708***    1.316***  
                                                     (0.184)     (0.210)     (0.268)    
      Occup. class: Upper white collar/Blue collar   -2.448***               -2.328***  
                                                     (0.327)                 (0.357)    
      Occup. class: Other white collar/Blue collar   -1.080***               -1.015**   
                                                     (0.315)                 (0.323)    
      Occup. class: Farmer/Blue collar               -0.622                  -0.792*    
                                                     (0.362)                 (0.383)    
      Income: under $500/$2000-2999                              -0.708      -0.662     
                                                                 (0.737)     (1.056)    
      Income: $500-$999/$2000-2999                               -0.238       0.912     
                                                                 (0.607)     (1.143)    
      Income: $1000-1999/$2000-2999                              -0.115       0.144     
                                                                 (0.343)     (0.440)    
      Income: $3000-3999/$2000-2999                              -0.807**    -0.527     
                                                                 (0.289)     (0.338)    
      Income: $4000-4999/$2000-2999                              -0.875*     -0.509     
                                                                 (0.358)     (0.411)    
      Income: $5000 and over/$2000-2999                          -1.470***   -0.535     
                                                                 (0.337)     (0.405)    
    ------------------------------------------------------------------------------------
      Nagelkerke R-sq.                                0.246       0.085       0.274     
      Deviance                                      404.190     524.433     390.551     
      AIC                                           412.190     538.433     410.551     
      N                                             344         398         340         
    ====================================================================================
      Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  

The comparison of the pseudo-R-Square values of model 1 and 2 suggests
that occupational class has a stronger influence on a preference for
Truman than household income. Indeed, if occupational class is taken
into account, the effect of income is no longer statistically
significant as the column corresponding to model 3 indicates.

Second, we compare the effect of occupational class and religious
denomination on the preference for Truman along the same lines as above.
We use `mtable` to collect the estimates of `model1`, `model4`, and
`model5` into a common table.

``` r
relabel(mtable(
              "Model 1"=model1,
              "Model 4"=model4,
              "Model 5"=model5,
              summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N")),
            occup4="Occup. class",
            relig3="Religion",
            gsub=TRUE
            )
```

    Calls:
    Model 1: glm(formula = (vote3 == "Truman") ~ occup4, family = "binomial", 
        data = vote.48)
    Model 4: glm(formula = (vote3 == "Truman") ~ relig3, family = "binomial", 
        data = vote.48)
    Model 5: glm(formula = (vote3 == "Truman") ~ occup4 + relig3, family = "binomial", 
        data = vote.48)

    ====================================================================================
                                                     Model 1     Model 4     Model 5    
    ------------------------------------------------------------------------------------
      (Intercept)                                     1.047***   -0.213       0.698**   
                                                     (0.184)     (0.126)     (0.216)    
      Occup. class: Upper white collar/Blue collar   -2.448***               -2.385***  
                                                     (0.327)                 (0.337)    
      Occup. class: Other white collar/Blue collar   -1.080***               -1.098***  
                                                     (0.315)                 (0.326)    
      Occup. class: Farmer/Blue collar               -0.622                  -0.346     
                                                     (0.362)                 (0.374)    
      Religion: Catholic/Protestant                               0.877***    0.685*    
                                                                 (0.243)     (0.292)    
      Religion: Other,none/Protestant                             0.975**     1.191**   
                                                                 (0.347)     (0.441)    
    ------------------------------------------------------------------------------------
      Nagelkerke R-sq.                                0.246       0.060       0.281     
      Deviance                                      404.190     537.711     393.105     
      AIC                                           412.190     543.711     405.105     
      N                                             344         402         344         
    ====================================================================================
      Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05  

A comparison of the pseudo-R-squared values suggests that also the
effect of religious denomination is weaker than that of occupational
class. However, as the third column in the above table indicates the
effect of religious denomination remains statistically significant.

------------------------------------------------------------------------

1.  National Election Studies, 1948: *Post-Election Study \[dataset\].*
    Ann Arbor, MI: University of Michigan, Center for Political Studies
    \[producer and distributor\], 1999. ANES Dataset ID: 1948.T; ICPSR
    Study Number: 7218. These materials are based on work supported by
    the National Science Foundation under Grant Nos.: SBR-9707741,
    SBR-9317631, SES-9209410, SES-9009379, SES-8808361, SES-8341310,
    SES-8207580, and SOC77-08885. Any opinions, findings and conclusions
    or recommendations expressed in these materials are those of the
    author(s) and do not necessarily reflect those of the National
    Science Foundation.
