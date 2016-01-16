\name{format_html}
\alias{show_html}
\alias{format_html}
\alias{write_html}
\alias{df_format_stdstyle}
\alias{mat_format_stdstyle}
\alias{format_html.data.frame}
\alias{format_html.matrix}
\title{
 Format Objects in HTML, show the HTML Format or Write it to a File
}
\description{
 \code{show_html} is for showing objects in a convenient way in HTML format. 
 \code{write_html} writes them in HTML format into a file.
 Both functions call the generic \code{format_html} for the format conversion.
}
\usage{
show_html(x, output = NULL, \dots)
write_html(x, file, \dots)

format_html(x, \dots)

\method{format_html}{data.frame}(x,
    toprule=2,midrule=1,bottomrule=2,
    split.dec=TRUE,
    row.names=TRUE,
    digits=getOption("digits"),
    format="f",
    style=df_format_stdstyle,
    margin="2ex auto", 
    \dots)
    
\method{format_html}{matrix}(x,
    toprule=2,midrule=1,bottomrule=2,
    split.dec=TRUE,
    digits=getOption("digits"),
    format="f",
    style=mat_format_stdstyle,
    margin="2ex auto", 
    \dots)
}
\arguments{
  \item{x}{an object.}
  \item{output}{character string or a function
    that determines how the HTML formatted object is shown.
  
  If \code{output} is a function, it is called with the path
  of a (temporary) file with HTML code, e.g. \emph{RStudio}'s \code{viewer}
  function (which is available in the package \code{rstudioapi}.
  
  If \code{output} equals "stdout", the HTML code is written to the 
  standard output stream (for use e.g. in output produced with \code{knitr}),
  if "file-show", the contents of a file with the HTML code is shown
  via \code{\link{file.show}}, and
  if "browser", the contents of a file with the HTML code is shown
  by the standard browser (via \code{\link{browseURL}}).
  
  This arguments has different defaults, depending of the type of
  the session. In non-interactive sessions, the default is
  "console", in interactive sessions other than RStudio,
  it is "browser", in interactive sessions with RStudio
  it is "file-show".
  
  These default sessions can be overriden by the option "html_viewer"
  (see \code{\link{options}}).
  }
  \item{file}{character string; name or path of the file where to
  write the HTML code to.}
  \item{toprule}{integer;
  thickness in pixels of rule at the top of the table.}
  \item{midrule}{integer;
  thickness in pixels of rules within the table.}
  \item{bottomrule}{integer;
  thickness in pixels of rule at the bottom of the table.}
  \item{split.dec}{logical; whether numbers should be centered
  at the decimal point by splitting the table cells.}
  \item{row.names}{logical; whether row names should be shown/exported.}
  \item{digits}{number of digits to be shown after the decimal dot. This is only useful, if
  the "ftable" object was created from a table created with \code{\link{genTable}} or the like.}
  \item{format}{a format string for \code{\link{formatC}}}
  \item{style}{string containing the stanard CSS styling of table cells.}
  \item{margin}{character string, determines the margin and thus
    the position of the HTML table.}
  \item{\dots}{other arguments, passed on to formatter functions.}
}
\value{
   \code{format_html} character string with code suitable for inclusion into a HTML-file.
}
