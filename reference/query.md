# Query an Object for Information

The function `query` can be used to search an object for a keyword.

The [`data.set`](https://melff.github.io/memisc/reference/dataSets.md)
and [`importer`](https://melff.github.io/memisc/reference/importers.md)
methods perform such a search through the annotations and value labels
of the items in the data set.

## Usage

``` r
query(x,pattern,...)
# S4 method for class 'data.set'
query(x,pattern,...)
# S4 method for class 'importer'
query(x,pattern,...)

# S4 method for class 'item'
query(x,pattern,...)
# (Called by the methods above.)
```

## Arguments

- x:

  an object

- pattern:

  a character string that gives the pattern to be searched for

- ...:

  optional arguments such as

  `fuzzy`

  :   logical, TRUE by default; use fuzzy search via
      [`agrep`](https://rdrr.io/r/base/agrep.html) or regexp search via
      [`grep`](https://rdrr.io/r/base/grep.html)

  `extended`

  :   logical, defaults to FALSE; passed to
      [`grep`](https://rdrr.io/r/base/grep.html)

  `perl`

  :   logical, defaults to TRUE; passed to
      [`grep`](https://rdrr.io/r/base/grep.html)

  `fixed`

  :   logical, defaults to TRUE; passed to
      [`grep`](https://rdrr.io/r/base/grep.html)

  `ignore.case`

  :   logical, defaults to TRUE; passed to
      [`grep`](https://rdrr.io/r/base/grep.html) or
      [`agrep`](https://rdrr.io/r/base/agrep.html)

  `insertions`

  :   numerical value, defaults to 0.999999999; passed to
      [`agrep`](https://rdrr.io/r/base/agrep.html)

  `deletions`

  :   numerical value, defaults to 0; passed to
      [`agrep`](https://rdrr.io/r/base/agrep.html)

  `substitutions`

  :   numerical value, defaults to 0; passed to
      [`agrep`](https://rdrr.io/r/base/agrep.html)

## Value

If both the annotation and the value labels of an item match the pattern
the `query` method for 'item' objects returns a list containing the
annotation and the value labels, otherwise if only the annotation or the
value labels match the pattern, either the annotation or the value
labels are returned, otherwise if neither matches the pattern, `query`
returns `NULL`.

The methods of `query` for 'data.set' and 'importer' objects return a
list of all non-`NULL` query results of all items contained by these
objects, or `NULL`.

## Examples

``` r
nes1948.por <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())
nes1948 <- spss.portable.file(nes1948.por)
#> Warning: 9 variables have duplicated labels:
#>   V480004, V480012, V480020, V480021A, V480021B, V480033A, V480033B,
#>   V480034A, V480034B
query(nes1948,"TRUMAN")
#> $V480013
#> 
#>  Values and labels:
#> 
#>    10 'SURPRISED -- THOUGHT PEOPLE WANTED CHA'
#>    11 'SURPRISED -- EXPERTS, RADIO, PAPERS SA'
#>    12 'SURPRISED -- DEWEY A BETTER MAN'       
#>    13 'SURPRISED -- TRUMAN RECORD BAD'        
#>    14 'SURPRISED -- OTHER, MISCELLANEOUS'     
#>    15 'SURPRISED -- DON'T KNOW WHY'           
#>    19 'SURPRISED -- NA WHY'                   
#>    20 'EXPECTED -- PEOPLE WANTED SAME ADMINIS'
#>    21 'EXPECTED -- SKEPTICAL OF RADIO, NEWSPA'
#>    22 'EXPECTED -- REPUBLICANS OVER-CONFIDENT'
#>    23 'EXPECTED -- TRUMAN'S CAMPAIGN MORE EFF'
#>    24 'EXPECTED -- TRUMAN A BETTER MAN OR BET'
#>    25 'EXPECTED -- PEOPLE OPPOSED TO 80TH CON'
#>    26 'EXPECTED -- PEOPLE VOTED IN ROOSEVELT' 
#>    27 'EXPECTED -- LABOR, NEGRO, GOVERNMENT E'
#>    28 'EXPECTED -- DK WHY'                    
#>    29 'EXPECTED -- NA WHY'                    
#>    30 'EXPECTED -- OTHER'                     
#>    88 'IGNORES POLITICS'                      
#>    98 'DON'T KNOW WHETHER SURPRISED'          
#>    99 'NA WHETHER SURPRISED'                  
#> 
#> 
#> $V480014A
#> $V480014A$annotation
#> description:
#>      WHY PPL VTD FOR TRUMAN 1
#> 
#> 
#> $V480014A$labels
#> 
#>  Values and labels:
#> 
#>    10 'BETTER MAN'                             
#>    20 'EXPERIENCED, GOOD RECORD'               
#>    30 'TRUMAN PRO-LABOR, NEGRO, WORKING MAN'   
#>    40 'GOOD CAMPAIGN CONDUCTED BY TRUMAN'      
#>    50 'DEMOCRATS MEAN PROSPERITY, REPUBLICANS' 
#>    60 'TRUMAN PRO RENT CONTROL, PRICE CONTROL,'
#>    70 'ROOSEVELT TRADITION'                    
#>    80 'PERSONAL ATTRIBUTES'                    
#>    90 'OTHER'                                  
#>    91 'NO SECOND REASON'                       
#>    98 'DK'                                     
#>    99 'NA'                                     
#> 
#> 
#> 
#> $V480014B
#> $V480014B$annotation
#> description:
#>      WHY PPL VTD FOR TRUMAN 2
#> 
#> 
#> $V480014B$labels
#> 
#>  Values and labels:
#> 
#>    10 'BETTER MAN'                             
#>    20 'EXPERIENCED, GOOD RECORD'               
#>    30 'TRUMAN PRO-LABOR, NEGRO, WORKING MAN'   
#>    40 'GOOD CAMPAIGN CONDUCTED BY TRUMAN'      
#>    50 'DEMOCRATS MEAN PROSPERITY, REPUBLICANS' 
#>    60 'TRUMAN PRO RENT CONTROL, PRICE CONTROL,'
#>    70 'ROOSEVELT TRADITION'                    
#>    80 'PERSONAL ATTRIBUTES'                    
#>    90 'OTHER'                                  
#>    91 'NO SECOND REASON'                       
#>    98 'DK'                                     
#>    99 'NA'                                     
#> 
#> 
#> 
#> $V480015A
#> $V480015A$annotation
#> description:
#>      WHY PPL VTD AGNST TRUMAN 1
#> 
#> 
#> $V480015A$labels
#> 
#>  Values and labels:
#> 
#>    10 'SMALL MAN, INADEQUATE BACKGROUND'       
#>    20 'CAN'T HANDLE SUBORDINATES, OR PICKED BA'
#>    30 'CAN'T GET THINGS DONE'                  
#>    40 'PARTIAL TO LABOR, NEGRO, WORKING MAN, G'
#>    50 'CAN'T GET ALONG WITH CONGRESS'          
#>    60 'CIVIL RIGHTS PROGRAM'                   
#>    70 'NOTHING SPECIAL ABOUT TRUMAN MADE PEOPL'
#>    80 'PERSONAL ATTRIBUTES'                    
#>    90 'OTHER REASONS'                          
#>    91 'NO SECOND REASON'                       
#>    98 'DK'                                     
#>    99 'NA'                                     
#> 
#> 
#> 
#> $V480015B
#> $V480015B$annotation
#> description:
#>      WHY PPL VTD AGNST TRUMAN 2
#> 
#> 
#> $V480015B$labels
#> 
#>  Values and labels:
#> 
#>    10 'SMALL MAN, INADEQUATE BACKGROUND'       
#>    20 'CAN'T HANDLE SUBORDINATES, OR PICKED BA'
#>    30 'CAN'T GET THINGS DONE'                  
#>    40 'PARTIAL TO LABOR, NEGRO, WORKING MAN, G'
#>    50 'CAN'T GET ALONG WITH CONGRESS'          
#>    60 'CIVIL RIGHTS PROGRAM'                   
#>    70 'NOTHING SPECIAL ABOUT TRUMAN MADE PEOPL'
#>    80 'PERSONAL ATTRIBUTES'                    
#>    90 'OTHER REASONS'                          
#>    91 'NO SECOND REASON'                       
#>    98 'DK'                                     
#>    99 'NA'                                     
#> 
#> 
#> 
#> $V480016A
#> 
#>  Values and labels:
#> 
#>    10 'BETTER MAN'                             
#>    20 'EXPERIENCED'                            
#>    30 'DEWEY PRO BUSINESS, WEALTHY, TRUMAN PRO'
#>    40 'GOOD CAMPAIGN CONDUCTED BY DEWEY'       
#>    50 'REPUBLICANS BRING PROSPERITY, GOOD FOR' 
#>    60 'DEWEY PRO-80TH CONGRESS, TAFT-HARTLEY A'
#>    70 'DEMOCRATS IN OFFICE TOO LONG'           
#>    80 'PERSONAL ATTRIBUTES'                    
#>    90 'OTHER REASONS'                          
#>    91 'NO SECOND REASON'                       
#>    98 'DK'                                     
#>    99 'NA'                                     
#> 
#> 
#> $V480016B
#> 
#>  Values and labels:
#> 
#>    10 'BETTER MAN'                             
#>    20 'EXPERIENCED'                            
#>    30 'DEWEY PRO BUSINESS, WEALTHY, TRUMAN PRO'
#>    40 'GOOD CAMPAIGN CONDUCTED BY DEWEY'       
#>    50 'REPUBLICANS BRING PROSPERITY, GOOD FOR' 
#>    60 'DEWEY PRO-80TH CONGRESS, TAFT-HARTLEY A'
#>    70 'DEMOCRATS IN OFFICE TOO LONG'           
#>    80 'PERSONAL ATTRIBUTES'                    
#>    90 'OTHER REASONS'                          
#>    91 'NO SECOND REASON'                       
#>    98 'DK'                                     
#>    99 'NA'                                     
#> 
#> 
#> $V480018
#> 
#>  Values and labels:
#> 
#>    1 'VOTED - FOR TRUMAN' 
#>    2 'VOTED - FOR DEWEY'  
#>    3 'VOTED - FOR WALLACE'
#>    4 'VOTED - FOR OTHER'  
#>    5 'VOTED - NA FOR WHOM'
#>    6 'DID NOT VOTE'       
#>    9 'NA WHETHER VOTED'   
#> 
#> 
#> $V480020
#> 
#>  Values and labels:
#> 
#>    11 'DID NOT CONSIDER ANYONE ELSE'
#>    12 'CONSIDERED DEWEY'            
#>    13 'CONSIDERED WALLACE'          
#>    14 'CONSIDERED OTHER'            
#>    19 'NA'                          
#>    21 'CONSIDERED TRUMAN'           
#>    22 'DID NOT CONSIDER ANYONE ELSE'
#>    23 'CONSIDERED WALLACE'          
#>    24 'CONSIDERED OTHER'            
#>    29 'NA'                          
#>    33 'DID NOT CONSIDER ANYONE ELSE'
#>    41 'CONSIDERED TRUMAN'           
#>    44 'DID NOT CONSIDER ANYONE ELSE'
#>    49 'NA'                          
#>    91 'INAP'                        
#> 
#> 
#> $V480023
#> 
#>  Values and labels:
#> 
#>    0 'INAP'              
#>    1 'TRUMAN - STRAIGHT' 
#>    2 'TRUMAN - SPLIT'    
#>    3 'DEWEY - STRAIGHT'  
#>    4 'DEWEY - SPLIT'     
#>    5 'WALLACE - STRAIGHT'
#>    6 'OTHER - STRAIGHT'  
#>    7 'OTHER - SPLIT'     
#>    8 'DK'                
#>    9 'NA'                
#> 
#> 
#> $V480024
#> 
#>  Values and labels:
#> 
#>    0 'INAP'   
#>    1 'TRUMAN' 
#>    2 'DEWEY'  
#>    3 'WALLACE'
#>    7 'OTHER'  
#>    8 'DK'     
#>    9 'NA'     
#> 
#> 
#> $V480031A
#> description:
#>      GRPS IDENTIFIED W TRUMAN 1
#> 
#> 
#> $V480031B
#> description:
#>      GRPS IDENTIFIED W TRUMAN 2
#> 
#> 
#> $V480031C
#> description:
#>      GRPS IDENTIFIED W TRUMAN 3
#> 
#> 
#> $V480037
#> 
#>  Values and labels:
#> 
#>    0 'NO INCIDENT MENTIONED'      
#>    1 'DEWEY - TRAIN ENGINEER'     
#>    2 'DEWEY - 3 SYLLABLE WORDS'   
#>    3 'DEWEY - DEBATE WITH STASSEN'
#>    4 'DEWEY - PRESS CONFERENCE'   
#>    5 'TRUMAN - VINSON-PRO'        
#>    6 'TRUMAN - VINSON-CON'        
#> 
#> 
```
