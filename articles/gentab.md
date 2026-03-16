# Generating Tables of Descriptive Statistics

## Motivation

*R* is well suited for statistical graphics, the application of advanced
data analysis techniques, and Monte Carlo studies of estimators.
However, it lacks support for the typical data management tasks as they
arise in the social sciences as well as for the simple generation of
desctiptive statistics. “memisc” facilitates not only typical data
management tasks of survey researchers, but also the generation of
descriptive statistics, as they are often a first step in serious social
science data analysis. In particular it facilitates the creation of
tables of percentages of other descriptive statistics broken down by
subgroups in the data. This is mainly achieved by the function
`genTable`, which is described in the following section. The section
thereafter describes how tables thus created can be exported to LaTeX
and HTML.

Note that these examples require data not included in the package (you
need to register to [GESIS](https://www.gesis.org) to download the
data). The vignette code cannot be run without this additional data.

## Creating Tables of Descriptive Statistics

General table of descriptive statistics can be created using the
function
[`genTable()`](https://melff.github.io/memisc/reference/genTable.md).
The syntax of calls to this function is quite similar to that of the
function [`xtabs()`](https://rdrr.io/r/stats/xtabs.html): The first
argument (tagged `formula`) is a formula that determines the descriptive
statistics used and by what groups they are computed. The left-hand side
of the formula determines the statistics being computed. The right-hand
side determines the grouping factor(s). The second argument is an
optional `data=` argument that determines from which data frame or data
set the descriptive statistics are to be computed. This is illustrated
by the following example, which uses (like the page on item objects, see
[`?item`](https://melff.github.io/memisc/reference/items.md)) the GLES
2013 election study[¹](#fn1). In this example we first create a table of
some descriptives of the age distribution of the respondents per German
federal state:

``` r
library(memisc)
ZA5702 <- spss.system.file("Data/ZA5702_v2-0-0.sav",
                           ignore.scale.info=TRUE) # Because the measurement info in the file is wrong.
gles2013work <- subset(ZA5702,
                       select=c(
                         wave                  = survey,
                         gender                = vn1,
                         byear                 = vn2c,
                         bmonth                = vn2b,
                         intent.turnout        = v10,
                         turnout               = n10,
                         voteint.candidate     = v11aa,
                         voteint.list          = v11ba,
                         postal.vote.candidate = v12aa,
                         postal.vote.list      = v12ba,
                         vote.candidate        = n11aa,
                         vote.list             = n11ba,                 
                         bula                  = bl
                       ))
gles2013work <- within(gles2013work,{
  measurement(byear) <- "interval"
  measurement(bmonth) <- "interval"
  age <- 2013 - byear
  age[bmonth > 9] <- age[bmonth > 9] - 1
  
})
options(digits=3)
age.tab <- genTable(c(Mean=mean(age),
           `Std.dev`=sd(age),
           Median=median(age))~bula,
         data=gles2013work)
age.tab
```

             bula
              Baden-Wuerttemberg Bayern Berlin Brandenburg Bremen Hamburg Hessen
      Mean                    55     54     53          60     60      51     57
      Std.dev                 19     19     20          19     12      19     19
      Median                  57     56     57          62     63      53     60
             bula
              Mecklenburg-Vorpommern Niedersachsen Nordrhein-Westfalen
      Mean                        57            55                  54
      Std.dev                     19            18                  19
      Median                      60            56                  55
             bula
              Rheinland-Pfalz Saarland Sachsen Sachsen-Anhalt Schleswig-Holstein
      Mean                 57       62      58             55                 60
      Std.dev              18       17      17             17                 20
      Median               60       65      60             56                 65
             bula
              Thueringen
      Mean            58
      Std.dev         17
      Median          60

This table does not look good, so we transprose it:

``` r
age.tab <- t(age.tab)
age.tab
```

    bula                     Mean Std.dev Median
      Baden-Wuerttemberg     54.5    18.9   57.0
      Bayern                 54.4    18.9   56.0
      Berlin                 52.8    19.8   57.0
      Brandenburg            59.7    19.3   62.5
      Bremen                 60.4    11.5   63.0
      Hamburg                51.5    18.7   53.0
      Hessen                 56.9    18.5   60.0
      Mecklenburg-Vorpommern 57.0    19.2   60.5
      Niedersachsen          55.1    18.4   56.0
      Nordrhein-Westfalen    53.9    19.1   55.0
      Rheinland-Pfalz        57.2    18.2   60.5
      Saarland               61.9    17.3   65.0
      Sachsen                58.3    16.7   60.5
      Sachsen-Anhalt         54.7    17.1   56.0
      Schleswig-Holstein     60.0    19.9   65.0
      Thueringen             57.8    17.4   60.0

In the next example we create a table of percentages of the second votes
per federal state. First we have to prepare the data, though:

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

    Warning messages:
    1: In cases(postal.vote.candidate <- wave == 1 & intent.turnout ==  :
      78 NAs created
    2: In cases(postal.vote.list <- wave == 1 & intent.turnout == 6, 900 <- wave ==  :
      78 NAs created
    3: In recode(as.item(candidate.vote), `CDU/CSU` = 1 <- 1, SPD = 2 <- 4,  :
      recoding created 18 NAs
    4: In recode(as.item(list.vote), `CDU/CSU` = 1 <- 1, SPD = 2 <- 4,  :
      recoding created 19 NAs

(When the code is run, some warnings are issued, that indicate that the
conditions are not exhaustive, that is, there are some observations for
which none of the conditions in the call
[`cases()`](https://melff.github.io/memisc/reference/cases.md) are met.
The corresponding elements of resulting vector will contain `NA` for
these observations. In the present case this occurs with observations
that have missing values in both `intent.turnout` and `turnout`.)

After having set up the data, we get our table of percentages:

``` r
vote.tab <- genTable(percent(list.vote)~bula,
                     data=gles2013work)
options(digits=1)
t(vote.tab)
```

    bula                     CDU/CSU SPD FDP Grüne Linke NPD Piraten AfD Other No Vote   N
      Baden-Wuerttemberg          28  22   7    17     6 0.4     2.1 4.6   1.1      12 285
      Bayern                      36  18   6    11     5 0.0     2.4 4.0   2.0      16 451
      Berlin                      27  22   8    10    14 1.8     1.8 6.6   0.6       8 166
      Brandenburg                 20  23   2     6    19 0.6     0.6 2.5   1.2      25 162
      Bremen                      22  26   0    17    13 0.0     0.0 4.3   0.0      17  23
      Hamburg                     22  36   2     4     7 2.2     0.0 4.4   2.2      20  45
      Hessen                      42  26   3     8     4 0.0     0.5 3.0   0.0      12 200
      Mecklenburg-Vorpommern      33  20   2     4    18 1.4     2.7 1.4   0.0      18 146
      Niedersachsen               33  32   3    10     3 0.0     0.7 0.7   0.4      17 284
      Nordrhein-Westfalen         33  31   3    11     4 0.4     2.3 1.8   0.7      13 563
      Rheinland-Pfalz             39  21   2     6     9 1.6     0.8 3.9   1.6      15 127
      Saarland                    40  40   0     0     0 0.0     0.0 0.0   0.0      20  30
      Sachsen                     49  17   1     3    14 0.3     1.2 0.9   0.3      13 332
      Sachsen-Anhalt              27  29   1     8    19 0.4     0.8 0.4   0.0      13 241
      Schleswig-Holstein          28  26   4     9     4 0.0     0.0 5.2   0.9      22 116
      Thueringen                  35  16   2     3    22 1.2     0.0 2.4   0.8      18 245

It is of course also possible to create multi-dimensional tables,
i.e. tables created by grouping by more than one factor:

``` r
gles2013work <- within(gles2013work,{

  # We relabel the items, since they are originally in German
  labels(turnout) <- c("Yes, voted"=1, "No, did not vote"=2)   
  labels(gender) <- c("Male"=1,"Female"=2)
})
genTable(percent(turnout)~gender+bula,
         data=gles2013work)
```

    , , bula = Baden-Wuerttemberg

                      gender
                       Male Female
      Yes, voted         88     85
      No, did not vote   12     15
      N                  90     61

    , , bula = Bayern

                      gender
                       Male Female
      Yes, voted         85     80
      No, did not vote   15     20
      N                  89    129

    , , bula = Berlin

                      gender
                       Male Female
      Yes, voted        100     85
      No, did not vote    0     15
      N                  38     52

    , , bula = Brandenburg

                      gender
                       Male Female
      Yes, voted         83     77
      No, did not vote   17     23
      N                  36     62

    , , bula = Bremen

                      gender
                       Male Female
      Yes, voted         91     80
      No, did not vote    9     20
      N                  11      5

    , , bula = Hamburg

                      gender
                       Male Female
      Yes, voted         88     76
      No, did not vote   12     24
      N                  16     21

    , , bula = Hessen

                      gender
                       Male Female
      Yes, voted         91     81
      No, did not vote    9     19
      N                  66     48

    , , bula = Mecklenburg-Vorpommern

                      gender
                       Male Female
      Yes, voted         84     72
      No, did not vote   16     28
      N                  32     47

    , , bula = Niedersachsen

                      gender
                       Male Female
      Yes, voted         88     83
      No, did not vote   12     17
      N                  75     70

    , , bula = Nordrhein-Westfalen

                      gender
                       Male Female
      Yes, voted         90     82
      No, did not vote   10     18
      N                 148    158

    , , bula = Rheinland-Pfalz

                      gender
                       Male Female
      Yes, voted         84     85
      No, did not vote   16     15
      N                  43     34

    , , bula = Saarland

                      gender
                       Male Female
      Yes, voted         91     72
      No, did not vote    9     28
      N                  11     18

    , , bula = Sachsen

                      gender
                       Male Female
      Yes, voted         88     88
      No, did not vote   12     12
      N                 103     73

    , , bula = Sachsen-Anhalt

                      gender
                       Male Female
      Yes, voted         89     81
      No, did not vote   11     19
      N                  63     73

    , , bula = Schleswig-Holstein

                      gender
                       Male Female
      Yes, voted         89     85
      No, did not vote   11     15
      N                  37     33

    , , bula = Thueringen

                      gender
                       Male Female
      Yes, voted         91     71
      No, did not vote    9     29
      N                  70     73

## Formatting Tables of Descriptive Statistics

The results of
[`genTable()`](https://melff.github.io/memisc/reference/genTable.md) are
objects of class `"table"` so that they can be re-arranged into a
“flattened” table by the function `ftable`. To demonstrate this, we
continue the previous example:

``` r
gt <- genTable(percent(turnout)~gender+bula,
         data=gles2013work)
# We beautify the table a bit ...
names(dimnames(gt)) <- c("Voted","Gender","State")
gt <- dimrename(gt,"Yes, voted"="Yes",
                "No, did not vote"="No")
ftable(gt,col.vars = c("Gender","Voted"))
```

                           Gender Male         Female        
                           Voted   Yes  No   N    Yes  No   N
    State                                                    
    Baden-Wuerttemberg              88  12  90     85  15  61
    Bayern                          85  15  89     80  20 129
    Berlin                         100   0  38     85  15  52
    Brandenburg                     83  17  36     77  23  62
    Bremen                          91   9  11     80  20   5
    Hamburg                         88  12  16     76  24  21
    Hessen                          91   9  66     81  19  48
    Mecklenburg-Vorpommern          84  16  32     72  28  47
    Niedersachsen                   88  12  75     83  17  70
    Nordrhein-Westfalen             90  10 148     82  18 158
    Rheinland-Pfalz                 84  16  43     85  15  34
    Saarland                        91   9  11     72  28  18
    Sachsen                         88  12 103     88  12  73
    Sachsen-Anhalt                  89  11  63     81  19  73
    Schleswig-Holstein              89  11  37     85  15  33
    Thueringen                      91   9  70     71  29  73

Arranging the cells of a table using
[`ftable()`](https://rdrr.io/r/stats/ftable.html) improves the
appearance of the results of
[`genTable()`](https://melff.github.io/memisc/reference/genTable.md) on
screen, but to include the results into a word processor document or a
LaTeX file, further facilities are needed and provided by “memisc”. To
include the flattened table into a LaTeX document, one can convert and
store it in the appropriate format using
[`toLatex()`](https://rdrr.io/r/utils/toLatex.html) and
[`writeLines()`](https://rdrr.io/r/base/writeLines.html)

``` r
ft <- ftable(gt,col.vars = c("Gender","Voted"))
lt <- toLatex(ft,digits=c(1,1,0,1,1,0))
writeLines(lt,con="Voted2013-GenderState.tex")
```

For HTML output, one can use
[`show_html()`](https://melff.github.io/memisc/reference/format_html.md)
(e.g. for inclusion in “knitr” documents) and
[`write_html()`](https://melff.github.io/memisc/reference/format_html.md),
both functions being based on
[`format_html()`](https://melff.github.io/memisc/reference/format_html.md).
Here we continue the example to demonstate this:

``` r
show_html(ft,digits=c(1,1,0,1,1,0))
```

|                        |         |      |     |     |     |     |     |     |     |     |        |     |     |     |     |     |     |     |     |
|------------------------|---------|------|-----|-----|-----|-----|-----|-----|-----|-----|--------|-----|-----|-----|-----|-----|-----|-----|-----|
|                        | Gender: | Male |     |     |     |     |     |     |     |     | Female |     |     |     |     |     |     |     |     |
| State                  | Voted:  | Yes  |     |     | No  |     |     | N   |     |     | Yes    |     |     | No  |     |     | N   |     |     |
| Baden-Wuerttemberg     |         | 87   | .   | 8   | 12  | .   | 2   | 90  |     |     | 85     | .   | 2   | 14  | .   | 8   | 61  |     |     |
| Bayern                 |         | 85   | .   | 4   | 14  | .   | 6   | 89  |     |     | 79     | .   | 8   | 20  | .   | 2   | 129 |     |     |
| Berlin                 |         | 100  | .   | 0   | 0   | .   | 0   | 38  |     |     | 84     | .   | 6   | 15  | .   | 4   | 52  |     |     |
| Brandenburg            |         | 83   | .   | 3   | 16  | .   | 7   | 36  |     |     | 77     | .   | 4   | 22  | .   | 6   | 62  |     |     |
| Bremen                 |         | 90   | .   | 9   | 9   | .   | 1   | 11  |     |     | 80     | .   | 0   | 20  | .   | 0   | 5   |     |     |
| Hamburg                |         | 87   | .   | 5   | 12  | .   | 5   | 16  |     |     | 76     | .   | 2   | 23  | .   | 8   | 21  |     |     |
| Hessen                 |         | 90   | .   | 9   | 9   | .   | 1   | 66  |     |     | 81     | .   | 2   | 18  | .   | 8   | 48  |     |     |
| Mecklenburg-Vorpommern |         | 84   | .   | 4   | 15  | .   | 6   | 32  |     |     | 72     | .   | 3   | 27  | .   | 7   | 47  |     |     |
| Niedersachsen          |         | 88   | .   | 0   | 12  | .   | 0   | 75  |     |     | 82     | .   | 9   | 17  | .   | 1   | 70  |     |     |
| Nordrhein-Westfalen    |         | 89   | .   | 9   | 10  | .   | 1   | 148 |     |     | 82     | .   | 3   | 17  | .   | 7   | 158 |     |     |
| Rheinland-Pfalz        |         | 83   | .   | 7   | 16  | .   | 3   | 43  |     |     | 85     | .   | 3   | 14  | .   | 7   | 34  |     |     |
| Saarland               |         | 90   | .   | 9   | 9   | .   | 1   | 11  |     |     | 72     | .   | 2   | 27  | .   | 8   | 18  |     |     |
| Sachsen                |         | 88   | .   | 3   | 11  | .   | 7   | 103 |     |     | 87     | .   | 7   | 12  | .   | 3   | 73  |     |     |
| Sachsen-Anhalt         |         | 88   | .   | 9   | 11  | .   | 1   | 63  |     |     | 80     | .   | 8   | 19  | .   | 2   | 73  |     |     |
| Schleswig-Holstein     |         | 89   | .   | 2   | 10  | .   | 8   | 37  |     |     | 84     | .   | 8   | 15  | .   | 2   | 33  |     |     |
| Thueringen             |         | 91   | .   | 4   | 8   | .   | 6   | 70  |     |     | 71     | .   | 2   | 28  | .   | 8   | 73  |     |     |

``` r
show_html(ft,digits=c(1,1,0,1,1,0),show.titles=FALSE)
```

|                        |      |     |     |     |     |     |     |     |     |        |     |     |     |     |     |     |     |     |
|------------------------|------|-----|-----|-----|-----|-----|-----|-----|-----|--------|-----|-----|-----|-----|-----|-----|-----|-----|
|                        | Male |     |     |     |     |     |     |     |     | Female |     |     |     |     |     |     |     |     |
|                        | Yes  |     |     | No  |     |     | N   |     |     | Yes    |     |     | No  |     |     | N   |     |     |
| Baden-Wuerttemberg     | 87   | .   | 8   | 12  | .   | 2   | 90  |     |     | 85     | .   | 2   | 14  | .   | 8   | 61  |     |     |
| Bayern                 | 85   | .   | 4   | 14  | .   | 6   | 89  |     |     | 79     | .   | 8   | 20  | .   | 2   | 129 |     |     |
| Berlin                 | 100  | .   | 0   | 0   | .   | 0   | 38  |     |     | 84     | .   | 6   | 15  | .   | 4   | 52  |     |     |
| Brandenburg            | 83   | .   | 3   | 16  | .   | 7   | 36  |     |     | 77     | .   | 4   | 22  | .   | 6   | 62  |     |     |
| Bremen                 | 90   | .   | 9   | 9   | .   | 1   | 11  |     |     | 80     | .   | 0   | 20  | .   | 0   | 5   |     |     |
| Hamburg                | 87   | .   | 5   | 12  | .   | 5   | 16  |     |     | 76     | .   | 2   | 23  | .   | 8   | 21  |     |     |
| Hessen                 | 90   | .   | 9   | 9   | .   | 1   | 66  |     |     | 81     | .   | 2   | 18  | .   | 8   | 48  |     |     |
| Mecklenburg-Vorpommern | 84   | .   | 4   | 15  | .   | 6   | 32  |     |     | 72     | .   | 3   | 27  | .   | 7   | 47  |     |     |
| Niedersachsen          | 88   | .   | 0   | 12  | .   | 0   | 75  |     |     | 82     | .   | 9   | 17  | .   | 1   | 70  |     |     |
| Nordrhein-Westfalen    | 89   | .   | 9   | 10  | .   | 1   | 148 |     |     | 82     | .   | 3   | 17  | .   | 7   | 158 |     |     |
| Rheinland-Pfalz        | 83   | .   | 7   | 16  | .   | 3   | 43  |     |     | 85     | .   | 3   | 14  | .   | 7   | 34  |     |     |
| Saarland               | 90   | .   | 9   | 9   | .   | 1   | 11  |     |     | 72     | .   | 2   | 27  | .   | 8   | 18  |     |     |
| Sachsen                | 88   | .   | 3   | 11  | .   | 7   | 103 |     |     | 87     | .   | 7   | 12  | .   | 3   | 73  |     |     |
| Sachsen-Anhalt         | 88   | .   | 9   | 11  | .   | 1   | 63  |     |     | 80     | .   | 8   | 19  | .   | 2   | 73  |     |     |
| Schleswig-Holstein     | 89   | .   | 2   | 10  | .   | 8   | 37  |     |     | 84     | .   | 8   | 15  | .   | 2   | 33  |     |     |
| Thueringen             | 91   | .   | 4   | 8   | .   | 6   | 70  |     |     | 71     | .   | 2   | 28  | .   | 8   | 73  |     |     |

``` r
# Writing into a HTML file ...
write_html(ft,digits=c(1,1,0,1,1,0),show.titles=FALSE,
           file="Voted2013-GenderState.html")
```

Continuing another example:

``` r
# age.tab was created earlier
age.ftab <- ftable(age.tab,row.vars=2)
show_html(age.ftab,digits=1,show.titles=FALSE)
```

|                        |      |     |     |         |     |     |        |     |     |
|------------------------|------|-----|-----|---------|-----|-----|--------|-----|-----|
|                        | Mean |     |     | Std.dev |     |     | Median |     |     |
| Baden-Wuerttemberg     | 54   | .   | 5   | 18      | .   | 9   | 57     | .   | 0   |
| Bayern                 | 54   | .   | 4   | 18      | .   | 9   | 56     | .   | 0   |
| Berlin                 | 52   | .   | 8   | 19      | .   | 8   | 57     | .   | 0   |
| Brandenburg            | 59   | .   | 7   | 19      | .   | 3   | 62     | .   | 5   |
| Bremen                 | 60   | .   | 4   | 11      | .   | 5   | 63     | .   | 0   |
| Hamburg                | 51   | .   | 5   | 18      | .   | 7   | 53     | .   | 0   |
| Hessen                 | 56   | .   | 9   | 18      | .   | 5   | 60     | .   | 0   |
| Mecklenburg-Vorpommern | 57   | .   | 0   | 19      | .   | 2   | 60     | .   | 5   |
| Niedersachsen          | 55   | .   | 1   | 18      | .   | 4   | 56     | .   | 0   |
| Nordrhein-Westfalen    | 53   | .   | 9   | 19      | .   | 1   | 55     | .   | 0   |
| Rheinland-Pfalz        | 57   | .   | 2   | 18      | .   | 2   | 60     | .   | 5   |
| Saarland               | 61   | .   | 9   | 17      | .   | 3   | 65     | .   | 0   |
| Sachsen                | 58   | .   | 3   | 16      | .   | 7   | 60     | .   | 5   |
| Sachsen-Anhalt         | 54   | .   | 7   | 17      | .   | 1   | 56     | .   | 0   |
| Schleswig-Holstein     | 60   | .   | 0   | 19      | .   | 9   | 65     | .   | 0   |
| Thueringen             | 57   | .   | 8   | 17      | .   | 4   | 60     | .   | 0   |

Of course we can also export to LaTeX:

``` r
toLatex(age.ftab,digits=1,show.titles=FALSE)
```

    \begin{tabular}{llD{.}{.}{1}D{.}{.}{1}D{.}{.}{1}}
    \toprule
     && \multicolumn{1}{c}{Mean}&\multicolumn{1}{c}{Std.dev}&\multicolumn{1}{c}{Median}\\
    \midrule
    Baden-Wuerttemberg     && 54.5 & 18.9 & 57.0\\
    Bayern                 && 54.4 & 18.9 & 56.0\\
    Berlin                 && 52.8 & 19.8 & 57.0\\
    Brandenburg            && 59.7 & 19.3 & 62.5\\
    Bremen                 && 60.4 & 11.5 & 63.0\\
    Hamburg                && 51.5 & 18.7 & 53.0\\
    Hessen                 && 56.9 & 18.5 & 60.0\\
    Mecklenburg-Vorpommern && 57.0 & 19.2 & 60.5\\
    Niedersachsen          && 55.1 & 18.4 & 56.0\\
    Nordrhein-Westfalen    && 53.9 & 19.1 & 55.0\\
    Rheinland-Pfalz        && 57.2 & 18.2 & 60.5\\
    Saarland               && 61.9 & 17.3 & 65.0\\
    Sachsen                && 58.3 & 16.7 & 60.5\\
    Sachsen-Anhalt         && 54.7 & 17.1 & 56.0\\
    Schleswig-Holstein     && 60.0 & 19.9 & 65.0\\
    Thueringen             && 57.8 & 17.4 & 60.0\\
    \bottomrule
    \end{tabular}

------------------------------------------------------------------------

1.  The [German Longitudinal Election
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
