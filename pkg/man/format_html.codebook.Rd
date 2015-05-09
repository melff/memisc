\name{format_html.codebook}
\alias{format_html.codebook}
\title{
  Format Codebooks as HTML
}
\description{
  This is the method of \code{\link{format_html}} for "codebook" objects as created
  by the eponymous function (see \code{\link{codebook}})
}
\usage{
\method{format_html}{codebook}(x, 
    toprule = 2, midrule = 1, 
    padding = 3, 
    var_tag = "code", 
    varid_prefix = "", title_tag = "p",\dots)
}
\arguments{
  \item{x}{a "codebook" object}
  \item{toprule}{a non-negative integer; thickness of the line (in pixels) at the top of each
  codebook entry}
  \item{midrule}{a non-negative integer; thickness of the line (in pixels) that separates the
  header of an codebook entry from its body}
  \item{padding}{a non-negative integer; left-hand padding in "ex" of the 
  codebook entry contents}
  \item{var_tag}{character string; the HTML tag that contains the 
  name of the variable}
  \item{varid_prefix}{character string; a prefix added to 
  the anchor IDs of the code entry titles (to facilitate the 
  creation of tables of contents etc.)}
  \item{title_tag}{character string; the HTML tag that contains 
  the title of the codebook entry (the variable name and its description)}
 \item{\dots}{further arguments, ignored.}}
\seealso{
  See Also as \code{\link{format_html}}, \code{\link{show_html}}, \code{\link{write_html}}.
}

