# memisc 
### Tools for Managing Survey Data and Creating Tables of Estimates and Data Summaries

[![Travis build status](https://api.travis-ci.com/melff/memisc.svg?branch=master)](https://app.travis-ci.com/github/melff/memisc) 
[![Current release on GitHub](http://img.shields.io/github/release/melff/memisc.svg)](http://github.com/melff/memisc/releases/)
[![CRAN](http://www.r-pkg.org/badges/version-last-release/memisc)](http://cran.r-project.org/package=memisc)
[![Total downloads from RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/grand-total/memisc)](http://cran.r-project.org/web/packages/memisc/index.html)
[![Total downloads from RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/memisc)](http://cran.r-project.org/web/packages/memisc/index.html)

<!--[![Build status](https://ci.appveyor.com/api/projects/status/iav1id61lmlh7nkb?svg=true)](https://ci.appveyor.com/project/melff/memisc) one CI is enough .. -->

One of the aims of this package is to make life easier for useRs who deal with
survey data sets. It provides an infrastructure for the management of survey
data including [value labels](reference/labels.html), [definable missing values](reference/value-filters.html), [recoding](reference/recode.html) of variables,
production of [codebooks](reference/codebook.html), and [import of (subsets of) SPSS and Stata
files](reference/importers.html). Further, it provides functionality to produce [tables and data frames of
arbitrary descriptive statistics](reference/genTable.html) and (almost) publication-ready [tables of
regression model estimates](reference/mtable.html), which can be exported to [LaTeX](reference/mtable-format-latex.html) and [HTML](reference/mtable-format-html.html).

Releases will be published on [CRAN](http://cran.r-project.org/web/packages/memisc/). Pre-releases will be available on [GitHub](https://github.com/melff/memisc) in
source and binary form. To install from the sources on GitHub you can use
`remotes::install_github("melff/memisc",subdir="pkg")`.

```{=html}
<figure>
<figcaption>
An example for the output of <code>codebook()</code> in HTML format:
</figcaption>
<style>
    .cbe-body table {
         margin: unset;
    }
    .cbe-body table,
    .cbe-body table tr,
    .cbe-body table td
    {
         border-style: none;
         padding: unset;
    }
    .cbe-header {
         border-top: solid 2px;
         border-bottom: solid 1px;
         padding-left: 3ex;
    }
    .cbe-body {
         padding-left: 3ex;
    }
    .cbe-table {
         border-collapse: collapse;
    }
    .cbe-table td:nth-child(n of .header) {
         text-align: center;
         padding-left: .3em;
         padding-right: .3em;
    }
    .cbe-vl-table td:nth-child(1) {
         text-align: right;
         padding-left: .3em;
         padding-right: .3em;
    }
    .cbe-vl-table td:nth-child(2) {
         text-align: center;
         padding-left: .3em;
         padding-right: .3em;
    }
    .cbe-vl-table td:nth-child(3) {
         text-align: left;
         padding-left: .3em;
         padding-right: .3em;
    }
    .cbe-vl-table td:nth-child(4) {
         text-align: right;
         padding-left: .3em;
         padding-right: .3em;
    }
    .cbe-vl-table td:nth-child(5),
    .cbe-vl-table td:nth-child(8),
    .cbe-vl-table td:nth-child(11),
    .cbe-vl-table td:nth-child(14),
    .cbe-vl-table td:nth-child(17),
    .cbe-d-table td:nth-child(2),
    .cbe-d-table td:nth-child(5){
         text-align: right;
         padding-left: .3em;
         padding-right: 0px;
    }
    .cbe-vl-table td:nth-child(6),
    .cbe-vl-table td:nth-child(9),
    .cbe-vl-table td:nth-child(12),
    .cbe-vl-table td:nth-child(15),
    .cbe-vl-table td:nth-child(18),
    .cbe-d-table td:nth-child(3),
    .cbe-d-table td:nth-child(6) {
         text-align: center;
         padding-left: 0px;
         padding-right: 0px;
         width: 1px;
    }
    .cbe-vl-table td:nth-child(7),
    .cbe-vl-table td:nth-child(10),
    .cbe-vl-table td:nth-child(13),
    .cbe-vl-table td:nth-child(16),
    .cbe-vl-table td:nth-child(19),
    .cbe-d-table td:nth-child(4),
    .cbe-d-table td:nth-child(7) {
         text-align: left;
         padding-right: .3em;
         padding-left: 0px;
    }
    .cbe-d-table td:nth-child(1){
         text-align: left;
         padding-left: .3em;
         padding-right: .3em;
    }
</style>
<div class="codebook-entry">
<div class="cbe-header">
<p>
</p>
<p class="cbe-title" id="voteint">
<code class="cbe-name">voteint</code> — <span class="cbe-description">‘Vote intention’</span>
</p>
<p class="cbe-wording">
“Which party are you going to vote for in the general election next
Tuesday?”
</p>
</div>
<div class="cbe-body">
<p>
</p>
<table class="table cbe-spec"> <tbody><tr> <td> Storage mode: </td> <td> double
</td> </tr> <tr> <td> Measurement: </td> <td> nominal </td> </tr> <tr> <td>
Valid range: </td> <td> 1 - 9 </td> </tr> </tbody></table>
<p>
</p>
<table class="table cbe-table cbe-vl-table">
<tbody><tr> <td colspan="3" class="header"> Values and labels </td> <td
class="header"> N </td> <td colspan="3" class="header"> Valid </td> <td
colspan="3" class="header"> Total </td> </tr> <tr> <td> 1 </td> <td> </td> <td>
‘Cons’ </td> <td> 49 </td> <td> 27 </td> <td> .  </td> <td> 8 </td> <td> 24
</td> <td> .  </td> <td> 5 </td> </tr> <tr> <td> 2 </td> <td> </td> <td> ‘Lab’
</td> <td> 26 </td> <td> 14 </td> <td> .  </td> <td> 8 </td> <td> 13 </td> <td>
.  </td> <td> 0 </td> </tr> <tr> <td> 3 </td> <td> </td> <td> ‘LibDem’ </td>
<td> 21 </td> <td> 11 </td> <td> .  </td> <td> 9 </td> <td> 10 </td> <td> .
</td> <td> 5 </td> </tr> <tr> <td> 4 </td> <td> </td> <td> ‘Other’ </td> <td> 19
</td> <td> 10 </td> <td> .  </td> <td> 8 </td> <td> 9 </td> <td> .  </td> <td> 5
</td> </tr> <tr> <td> 9 </td> <td> </td> <td> ‘NoVote’ </td> <td> 61 </td> <td>
34 </td> <td> .  </td> <td> 7 </td> <td> 30 </td> <td> .  </td> <td> 5 </td>
</tr> <tr> <td> 97 </td> <td> M </td> <td> ‘DK’ </td> <td> 6 </td> <td> </td>
<td> </td> <td> </td> <td> 3 </td> <td> .  </td> <td> 0 </td> </tr> <tr> <td> 98
</td> <td> M </td> <td> ‘Refused’ </td> <td> 7 </td> <td> </td> <td> </td> <td>
</td> <td> 3 </td> <td> .  </td> <td> 5 </td> </tr> <tr> <td> 99 </td> <td> M
</td> <td> ‘N.a.’  </td> <td> 11 </td> <td> </td> <td> </td> <td> </td> <td> 5
</td> <td> .  </td> <td> 5 </td> </tr>
</tbody></table>
<p>
</p>
</div>
</div>
</figure>
```

```{=html}
<figure>
<figcaption>
An example of a table of model estimates produced with <code>mtable()</code>:
</figcaption>
<style>
      table#mtable-1 {
         border-collapse: collapse; border-style: none; margin: 2ex auto;
      } 
      table#mtable-1 tr {
          border-style: none;
      } 
      table#mtable-1 td {
        padding-top: 1px; 
        padding-bottom: 1px; 
        padding-left: 0.5ex; 
        padding-right: 0.5ex; 
        margin: 0px; 
        margin-top: 0px;
        margin-bottom: 0px;
        border-style: none; 
        border-width: 0px; 
      }
table#mtable-1 tr:nth-child(1){
     border-top: 2px solid;
}
table#mtable-1 tr:nth-child(1),
table#mtable-1 tr:nth-child(11){
     border-bottom: 1px solid;
}
table#mtable-1 tr:nth-child(16){
     border-bottom: 2px solid;
} table#mtable-1 td:nth-child(3),
table#mtable-1 td:nth-child(6),
table#mtable-1 td:nth-child(9){
         padding-left: 0px;
         padding-right: 0px;
         text-align: center; 
         /*background-color: blue;*/
        } table#mtable-1 td:nth-child(2),
table#mtable-1 td:nth-child(5),
table#mtable-1 td:nth-child(8){
         padding-left: 0.5ex;
         padding-right: 0px;
         text-align: right; 
         /*background-color: red;*/
        } table#mtable-1 td:nth-child(4),
table#mtable-1 td:nth-child(7),
table#mtable-1 td:nth-child(10){
         padding-left: 0px;
         padding-right: 0.5ex;
         text-align: left; 
         /*background-color: green;*/
        } 
table#mtable-1 td:nth-child(n of .header){
         padding-left: 0.5ex;
         padding-right: 0.5ex;
         text-align: center; 
         /*background-color: gray;*/
        } 
</style>
<table id="mtable-1" class="table">
<tr>
<td colspan="1" class="header">
</td>
<td colspan="3" class="header">
Model 1
</td>
<td colspan="3" class="header">
Model 2
</td>
<td colspan="3" class="header">
Model 3
</td>
</tr>
<tr>
<td>
Constant
</td>
<td>
30
</td>
<td>
.
</td>
<td>
628<span class="signif.symbol">***</span>
</td>
<td>
6
</td>
<td>
.
</td>
<td>
360<span class="signif.symbol">***</span>
</td>
<td>
28
</td>
<td>
.
</td>
<td>
566<span class="signif.symbol">***</span>
</td>
</tr>
<tr>
<td>
</td>
<td>
(7
</td>
<td>
.
</td>
<td>
409)
</td>
<td>
(1
</td>
<td>
.
</td>
<td>
252)
</td>
<td>
(7
</td>
<td>
.
</td>
<td>
355)
</td>
</tr>
<tr>
<td>
Percentage of population under 15
</td>
<td>
−0
</td>
<td>
.
</td>
<td>
471<span class="signif.symbol">**</span>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
−0
</td>
<td>
.
</td>
<td>
461<span class="signif.symbol">**</span>
</td>
</tr>
<tr>
<td>
</td>
<td>
(0
</td>
<td>
.
</td>
<td>
147)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0
</td>
<td>
.
</td>
<td>
145)
</td>
</tr>
<tr>
<td>
Percentage of population over 75
</td>
<td>
−1
</td>
<td>
.
</td>
<td>
934
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
−1
</td>
<td>
.
</td>
<td>
691
</td>
</tr>
<tr>
<td>
</td>
<td>
(1
</td>
<td>
.
</td>
<td>
041)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(1
</td>
<td>
.
</td>
<td>
084)
</td>
</tr>
<tr>
<td>
Real per-capita disposable income
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0
</td>
<td>
.
</td>
<td>
001
</td>
<td>
−0
</td>
<td>
.
</td>
<td>
000
</td>
</tr>
<tr>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0
</td>
<td>
.
</td>
<td>
001)
</td>
<td>
(0
</td>
<td>
.
</td>
<td>
001)
</td>
</tr>
<tr>
<td>
Growth rate of real per-capita disp. income
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0
</td>
<td>
.
</td>
<td>
529<span class="signif.symbol">*</span>
</td>
<td>
0
</td>
<td>
.
</td>
<td>
410<span class="signif.symbol">*</span>
</td>
</tr>
<tr>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0
</td>
<td>
.
</td>
<td>
210)
</td>
<td>
(0
</td>
<td>
.
</td>
<td>
196)
</td>
</tr>
<tr>
<td>
sigma
</td>
<td>
3
</td>
<td>
.
</td>
<td>
931
</td>
<td>
4
</td>
<td>
.
</td>
<td>
189
</td>
<td>
3
</td>
<td>
.
</td>
<td>
803
</td>
</tr>
<tr>
<td>
R-squared
</td>
<td>
0
</td>
<td>
.
</td>
<td>
262
</td>
<td>
0
</td>
<td>
.
</td>
<td>
162
</td>
<td>
0
</td>
<td>
.
</td>
<td>
338
</td>
</tr>
<tr>
<td>
F
</td>
<td>
8
</td>
<td>
.
</td>
<td>
332
</td>
<td>
4
</td>
<td>
.
</td>
<td>
528
</td>
<td>
5
</td>
<td>
.
</td>
<td>
756
</td>
</tr>
<tr>
<td>
p
</td>
<td>
0
</td>
<td>
.
</td>
<td>
001
</td>
<td>
0
</td>
<td>
.
</td>
<td>
016
</td>
<td>
0
</td>
<td>
.
</td>
<td>
001
</td>
</tr>
<tr>
<td>
N
</td>
<td>
50
</td>
<td>
</td>
<td>
</td>
<td>
50
</td>
<td>
</td>
<td>
</td>
<td>
50
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="10">
<p>
Significance: *** = p &lt; 0.001; ** = p &lt; 0.01; * = p &lt; 0.05
</p>
</td>
</tr>
</table>
</figure>
```
