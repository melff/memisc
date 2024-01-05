\name{format_html.ftable}
\alias{format_html.ftable}
\alias{format_html.ftable_matrix}
\alias{ftable_format_stdstyle}
\title{
  Format "Flattened Tables" as HTML
}
\description{
  This is the method of \code{\link{format_html}} for "ftable" objects (i.e. flattened
  contingency tables)
}
\usage{
\method{format_html}{ftable}(x, 
                   show.titles = TRUE, 
                   digits = 0, 
                   format = "f", 
                   toprule = 2, midrule = 1, bottomrule = 2, 
                   split.dec = TRUE, 
                   style = ftable_format_stdstyle,
                   margin="2ex auto", 
                   \dots)
\method{format_html}{ftable_matrix}(x,
                   show.titles=TRUE,
                   digits=0,
                   format="f",
                   toprule=2,midrule=1,bottomrule=2,
                   split.dec=TRUE,
                   style = ftable_format_stdstyle,
                   margin="2ex auto", 
                   varontop,
                   varinfront,
                   grouprules=1,
                   multi_digits=NULL,
                   \dots)
}
\arguments{
  \item{x}{an object of class \code{\link{ftable}}.}
  \item{show.titles}{logical; should the names of the cross-classified variables be shown?}
  \item{digits}{number of digits to be shown after the decimal dot. This is only useful, if
  the "ftable" object was created from a table created with \code{\link{genTable}} or the like.}
  \item{format}{a format string for \code{\link{formatC}}}
     \item{toprule}{integer;
     thickness in pixels of rule at the top of the table.}
     \item{midrule}{integer;
     thickness in pixels of rules within the table.}
     \item{bottomrule}{integer;
     thickness in pixels of rule at the bottom of the table.}
    \item{split.dec}{logical; whether numbers should be centered
    at the decimal point by splitting the table cells.}
    \item{style}{string containing the standard CSS styling of table cells.}
    \item{margin}{character string, determines the margin and thus
      the position of the HTML table.}
    \item{varontop}{logical; whether names of column variables should appear on top of factor levels}
    \item{varinfront}{logical; whether names of row variables should
      appear in front of factor levels}
    \item{grouprules}{integer, should be either 1 or 2; whether one or
      two rules should drawn to distinguish groups of rows.}
     \item{multi_digits}{NULL, a numeric vector, or a list. If it is a
          list it should have as many elements as
           the "ftable_matrix" contains columns, where each vector has
           as many columns as the respective "ftable". If it is a
           vector, it is put into a list with replicated elements
           according to the "ftable" components.
           The elements of these vectors can be used to specify a separate
           number of digits for each column of the respective "ftable".}
    \item{\dots}{further arguments, ignored.}}
\seealso{
  See Also as \code{\link{format_html}}, \code{\link{show_html}}, \code{\link{write_html}}.
}
