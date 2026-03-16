# Combining flattened contingency tables (and other tables)

This vignette demonstrates how flattened contingency tables can be
combined into more complex tables. For starters, we create a few
contingendy tables.

``` r
tab.Class.Age <- xtabs(Freq~Class+Age,data=Titanic)
tab.Survived.Class.Age <- xtabs(Freq~Survived+Class+Age,data=Titanic)
tab.Survived.Class.Sex <- xtabs(Freq~Survived+Class+Sex,data=Titanic)
tab.Survived.Class <- xtabs(Freq~Survived+Class,data=Titanic)
tab.Survived.Sex <- xtabs(Freq~Survived+Sex,data=Titanic)
tab.Survived.Age <- xtabs(Freq~Survived+Age,data=Titanic)
tab.Survived <- xtabs(Freq~Survived,data=Titanic)
```

Then we load the *memisc* package:

``` r
library(memisc)
```

Next, we create a few flattened contingency tables:

``` r
(ftab.Survived.Age <- ftable(tab.Survived.Age))
```

             Age Child Adult
    Survived                
    No              52  1438
    Yes             57   654

``` r
(ftab.Survived.Sex <- ftable(tab.Survived.Sex))
```

             Sex Male Female
    Survived                
    No           1364    126
    Yes           367    344

Since these tables have the same row variables, we can combine them
columnwise:

``` r
cbind(ftab.Survived.Age,
      ftab.Survived.Sex)
```

             Age Child Adult  Sex Male Female 
    Survived                                  
    No              52  1438      1364    126 
    Yes             57   654       367    344 
                                              

We can even add a simple table of counts:

``` r
cbind(ftab.Survived.Age,
      ftab.Survived.Sex,
      Total=tab.Survived)
```

             Age Child Adult  Sex Male Female   Total 
    Survived                                          
    No              52  1438      1364    126    1490 
    Yes             57   654       367    344     711 
                                                      

Of course, it is not enough to see such tables on screen, we also may
need them in a presentable format, such as LaTeX or HTML. For the latter
format we again have `format_html` and `show_html`. To see how this
works, we first look at the individual `ftable`s:

``` r
show_html(ftab.Survived.Age)
```

|          |     |       |     |     |       |     |     |
|----------|-----|-------|-----|-----|-------|-----|-----|
|          |     | Age   |     |     |       |     |     |
| Survived |     | Child |     |     | Adult |     |     |
| No       |     | 52    |     |     | 1438  |     |     |
| Yes      |     | 57    |     |     | 654   |     |     |

``` r
show_html(ftab.Survived.Sex)
```

|          |     |      |     |     |        |     |     |
|----------|-----|------|-----|-----|--------|-----|-----|
|          |     | Sex  |     |     |        |     |     |
| Survived |     | Male |     |     | Female |     |     |
| No       |     | 1364 |     |     | 126    |     |     |
| Yes      |     | 367  |     |     | 344    |     |     |

… and then at their combination

``` r
show_html(
  cbind(ftab.Survived.Age,
      ftab.Survived.Sex,
      Total=tab.Survived)
)
```

|          |     |       |     |     |       |     |     |      |     |     |        |     |     |       |     |     |
|----------|-----|-------|-----|-----|-------|-----|-----|------|-----|-----|--------|-----|-----|-------|-----|-----|
|          |     | Age   |     |     |       |     |     | Sex  |     |     |        |     |     |       |     |     |
|          |     | Child |     |     | Adult |     |     | Male |     |     | Female |     |     | Total |     |     |
| Survived | No  | 52    |     |     | 1438  |     |     | 1364 |     |     | 126    |     |     | 1490  |     |     |
|          | Yes | 57    |     |     | 654   |     |     | 367  |     |     | 344    |     |     | 711   |     |     |

To make, in “knitr”, the creation of HTML-versions automatic one can use
the following little trick:

``` r
knit_print.ftable_matrix <-function(x,options,...)
  knitr::asis_output(
    format_html(x,
                digits=if(length(options$ftable.digits))
                          options$ftable.digits
                       else 0,
                ...))
```

Now we do not need to call `show_html` while using “knitr”:

``` r
cbind(ftab.Survived.Age,
      ftab.Survived.Sex,
      Total=tab.Survived)
```

|          |     |       |     |     |       |     |     |      |     |     |        |     |     |       |     |     |
|----------|-----|-------|-----|-----|-------|-----|-----|------|-----|-----|--------|-----|-----|-------|-----|-----|
|          |     | Age   |     |     |       |     |     | Sex  |     |     |        |     |     |       |     |     |
|          |     | Child |     |     | Adult |     |     | Male |     |     | Female |     |     | Total |     |     |
| Survived | No  | 52    |     |     | 1438  |     |     | 1364 |     |     | 126    |     |     | 1490  |     |     |
|          | Yes | 57    |     |     | 654   |     |     | 367  |     |     | 344    |     |     | 711   |     |     |

This can be undone by removing the method function of `knit_print`:

``` r
rm(knit_print.ftable_matrix)
```

For `show_html` and `toLatex` there are some variants in how the
variable names are positioned, for example:

``` r
show_html(
  cbind(ftab.Survived.Age,
      ftab.Survived.Sex,
      Total=tab.Survived),
  varinfront=FALSE
)
```

|          |       |     |     |       |     |     |      |     |     |        |     |     |       |     |     |
|----------|-------|-----|-----|-------|-----|-----|------|-----|-----|--------|-----|-----|-------|-----|-----|
|          | Age   |     |     |       |     |     | Sex  |     |     |        |     |     |       |     |     |
| Survived | Child |     |     | Adult |     |     | Male |     |     | Female |     |     | Total |     |     |
| No       | 52    |     |     | 1438  |     |     | 1364 |     |     | 126    |     |     | 1490  |     |     |
| Yes      | 57    |     |     | 654   |     |     | 367  |     |     | 344    |     |     | 711   |     |     |

``` r
show_html(
  cbind(ftab.Survived.Age,
      ftab.Survived.Sex,
      Total=tab.Survived),
  varontop=FALSE
)
```

|          |     |      |       |     |     |       |     |     |      |      |     |     |        |     |     |       |     |     |
|----------|-----|------|-------|-----|-----|-------|-----|-----|------|------|-----|-----|--------|-----|-----|-------|-----|-----|
|          |     | Age: | Child |     |     | Adult |     |     | Sex: | Male |     |     | Female |     |     | Total |     |     |
| Survived | No  |      | 52    |     |     | 1438  |     |     |      | 1364 |     |     | 126    |     |     | 1490  |     |     |
|          | Yes |      | 57    |     |     | 654   |     |     |      | 367  |     |     | 344    |     |     | 711   |     |     |

Of course it is also possible to combine flat contingency tables
rowwise:

``` r
ftab.Age.Survived <- ftable(tab.Survived.Age,col.vars=1)
ftab.Sex.Survived <- ftable(tab.Survived.Sex,col.vars=1)
ftab.Class.Survived <- ftable(tab.Survived.Class,col.vars=1)

rbind(
  ftab.Age.Survived,
  ftab.Sex.Survived,
  ftab.Class.Survived,
  Total=tab.Survived
)
```

           Survived   No Yes 
    Age                      
    Child             52  57 
    Adult           1438 654 
                             
    Sex                      
    Male            1364 367 
    Female           126 344 
                             
    Class                    
    1st              122 203 
    2nd              167 118 
    3rd              528 178 
    Crew             673 212 
                             
                             
    Total           1490 711 
                             

``` r
show_html(
  rbind(
    ftab.Age.Survived,
    ftab.Sex.Survived,
    ftab.Class.Survived,
    Total=tab.Survived
  )
)
```

|       |        |          |     |     |     |     |     |
|-------|--------|----------|-----|-----|-----|-----|-----|
|       |        | Survived |     |     |     |     |     |
|       |        | No       |     |     | Yes |     |     |
| Age   | Child  | 52       |     |     | 57  |     |     |
|       | Adult  | 1438     |     |     | 654 |     |     |
| Sex   | Male   | 1364     |     |     | 367 |     |     |
|       | Female | 126      |     |     | 344 |     |     |
| Class | 1st    | 122      |     |     | 203 |     |     |
|       | 2nd    | 167      |     |     | 118 |     |     |
|       | 3rd    | 528      |     |     | 178 |     |     |
|       | Crew   | 673      |     |     | 212 |     |     |
| Total |        | 1490     |     |     | 711 |     |     |

It is also possible to create the `ftable`s from tables of percentages
or generic tables (created with `genTable`) etc.

``` r
ptab.Survived.Age<-percentages(Survived~Age,data=Titanic)
ptab.Survived.Sex<-percentages(Survived~Sex,data=Titanic)
ptab.Survived.Class<-percentages(Survived~Class,data=Titanic)

fptab.Age.Survived <- ftable(ptab.Survived.Age,col.vars=1)
fptab.Sex.Survived <- ftable(ptab.Survived.Sex,col.vars=1)
fptab.Class.Survived <- ftable(ptab.Survived.Class,col.vars=1)

show_html(
  rbind(
    fptab.Age.Survived,
    fptab.Sex.Survived,
    fptab.Class.Survived
  ),
  digits=1
)
```

|       |        |          |     |     |     |     |     |
|-------|--------|----------|-----|-----|-----|-----|-----|
|       |        | Survived |     |     |     |     |     |
|       |        | No       |     |     | Yes |     |     |
| Age   | Child  | 47       | .   | 7   | 52  | .   | 3   |
|       | Adult  | 68       | .   | 7   | 31  | .   | 3   |
| Sex   | Male   | 78       | .   | 8   | 21  | .   | 2   |
|       | Female | 26       | .   | 8   | 73  | .   | 2   |
| Class | 1st    | 37       | .   | 5   | 62  | .   | 5   |
|       | 2nd    | 58       | .   | 6   | 41  | .   | 4   |
|       | 3rd    | 74       | .   | 8   | 25  | .   | 2   |
|       | Crew   | 76       | .   | 0   | 24  | .   | 0   |

It is also possible to combine `rbind` and `cbind`:

``` r
tab.Age <- xtabs(Freq~Age,data=Titanic)
tab.Sex <- xtabs(Freq~Sex,data=Titanic)
tab.Class <- xtabs(Freq~Class,data=Titanic)


show_html(
  rbind(
    cbind(fptab.Age.Survived,Total=tab.Age),
    cbind(fptab.Sex.Survived,Total=tab.Sex),
    cbind(fptab.Class.Survived,Total=tab.Class)
  ),
  digits=c(1,0) # One digit after dot for percentages 
                # no digits for total counts.
)
```

|       |        |          |     |     |     |     |     |       |     |     |
|-------|--------|----------|-----|-----|-----|-----|-----|-------|-----|-----|
|       |        | Survived |     |     |     |     |     |       |     |     |
|       |        | No       |     |     | Yes |     |     | Total |     |     |
| Age   | Child  | 47       | .   | 7   | 52  | .   | 3   | 109   |     |     |
|       | Adult  | 68       | .   | 7   | 31  | .   | 3   | 2092  |     |     |
| Sex   | Male   | 78       | .   | 8   | 21  | .   | 2   | 1731  |     |     |
|       | Female | 26       | .   | 8   | 73  | .   | 2   | 470   |     |     |
| Class | 1st    | 37       | .   | 5   | 62  | .   | 5   | 325   |     |     |
|       | 2nd    | 58       | .   | 6   | 41  | .   | 4   | 285   |     |     |
|       | 3rd    | 74       | .   | 8   | 25  | .   | 2   | 706   |     |     |
|       | Crew   | 76       | .   | 0   | 24  | .   | 0   | 885   |     |     |

The same construct as LaTeX code:

``` r
toLatex(
  rbind(
    cbind(fptab.Age.Survived,Total=tab.Age),
    cbind(fptab.Sex.Survived,Total=tab.Sex),
    cbind(fptab.Class.Survived,Total=tab.Class)
  ),
  digits=c(1,0) # One digit after dot for percentages 
                # no digits for total counts.
)
```

    \begin{tabular}{llD{.}{.}{1}D{.}{.}{1}cD{.}{.}{0}}
    \toprule
    &&\multicolumn{2}{c}{Survived}&&\\
    \cmidrule{3-4}
    &&\multicolumn{1}{c}{No}&\multicolumn{1}{c}{Yes}&&\multicolumn{1}{c}{Total}\\
    \midrule
    Age & Child & 47.7 & 52.3 &  & 109\\
     & Adult & 68.7 & 31.3 &  & 2092\\
    \midrule Sex & Male & 78.8 & 21.2 &  & 1731\\
     & Female & 26.8 & 73.2 &  & 470\\
    \midrule Class & 1st & 37.5 & 62.5 &  & 325\\
     & 2nd & 58.6 & 41.4 &  & 285\\
     & 3rd & 74.8 & 25.2 &  & 706\\
     & Crew & 76.0 & 24.0 &  & 885\\
    \bottomrule
    \end{tabular}
