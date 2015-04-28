\name{format_html}
\alias{show_html}
\alias{format_html}
\alias{write_html}
\title{
 Format Objects in HTML, show the HTML Format or Write it to a File
}
\description{
 \code{show_html} is for showing objects (at present only
 'mtable' results) in a convenient way in HTML format. 
 \code{write_html} writes them in HTML format into a file.
 Both functions call the generic \code{format_html} for the format conversion.
}
\usage{
format_html(x, ...)
show_html(x, output = NULL, ...)
write_html(x, file, ...)
}
\arguments{
  \item{x}{an object.}
  \item{output}{character string; 
  describes how the HTML formatted object is shown.
  If "console", the HTML code is written to the 
  console (for use e.g. in output produced with \code{knitr}),
  if "file-show", the contents of a file with the HTML code is shown
  via \code{\link{file.show}}, and
  if "browser", the contents of a file with the HTML code is shown
  by the standard browser (via \code{\link{browseURL}}).
  
  This arguments has different defaults, depending of the type of
  the session. In non-interactive sessions, the default is
  "console", in interactive sessions other than RStudio,
  it is "browser", in interactive sessions with RStudio
  it is "file-show".
  
  These default sessions can be overriden by the option "show_html.output"
  (see \code{\link{options}}).
  }
  \item{file}{character string; name or path of the file where to
  write the HTML code to.}
  \item{\dots}{other arguments, passed on to formatter functions.}
}
\examples{
lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

mtable123 <- mtable("Model 1"=lm0,"Model 2"=lm1,"Model 3"=lm2,
                    summary.stats=c("sigma","R-squared","F","p","N"))

(mtable123 <- relabel(mtable123,
                      "(Intercept)" = "Constant",
                      pop15 = "Percentage of population under 15",
                      pop75 = "Percentage of population over 75",
                      dpi = "Real per-capita disposable income",
                      ddpi = "Growth rate of real per-capita disp. income"
))

show_html(mtable123)
}
