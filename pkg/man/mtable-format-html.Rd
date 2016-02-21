\name{mtable_format_html}
\alias{mtable_format_html}
\alias{format_html.mtable}
\alias{mtable_format_html}
\alias{mtable_format_stdstyle}
\title{HTML Formatting for 'mtable' Results}
\description{
  These functions formats 'mtable' objects into HTML format.
  }
\usage{
mtable_format_html(x,
                    interaction.sep = NULL,
                    toprule=2,midrule=1,bottomrule=2,
                    split.dec=TRUE,
                    style=mtable_format_stdstyle,
                    margin="2ex auto", 
                    force.names=FALSE,
                    \dots
  )
\method{format_html}{mtable}(x,
                    interaction.sep = NULL,
                    toprule=2,midrule=1,bottomrule=2,
                    split.dec=TRUE,
                    style=mtable_format_stdstyle,
                    margin="2ex auto", 
                    force.names=FALSE,
                    \dots
  )
}
\arguments{
     \item{x}{an object of class \code{mtable}}
     \item{toprule}{integer;
     thickness in pixels of rule at the top of the table.}
     \item{midrule}{integer;
     thickness in pixels of rules within the table.}
     \item{bottomrule}{integer;
     thickness in pixels of rule at the bottom of the table.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect or NULL. If NULL then a reasonable default is used
        (either a unicode character or an ampersand encoded HTML entity).}
    \item{split.dec}{logical; whether numbers should be centered
    at the decimal point by splitting the table cells.}
    \item{style}{string containing default the CSS styling.}
    \item{margin}{character string, determines the margin and thus
      the position of the HTML table.}
    \item{force.names}{logical; should model names be shown even
     if estimates of only one model are displayed?}
    \item{\dots}{further arguments, ignored. }
}
\value{
    A character string with code suitable for inclusion into a HTML-file.
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

