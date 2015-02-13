\name{mtable}
\alias{mtable}
\alias{relabel.mtable}
\alias{format.mtable}
\alias{print.mtable}
\alias{toLatex.mtable}
\alias{write.mtable}
\title{Comparative Table of Model Estimates}
\description{
  \code{mtable} produces a table of estimates for several models.
  }
\usage{
mtable(...,coef.style=getOption("coef.style"),
    summary.stats=TRUE,
    factor.style=getOption("factor.style"),
    getSummary=eval.parent(quote(getSummary)),
    float.style=getOption("float.style"),
    digits=min(3,getOption("digits")),
    drop=TRUE
  )
\method{relabel}{mtable}(x, \dots, gsub = FALSE, fixed = !gsub, warn = FALSE)

\method{format}{mtable}(x,
    coef.title="Coefficients",
    summary.title="Summaries",
    colsep="\t",
    rowsep="\n",
    trim=TRUE,
    trimleft=trim,
    trimright=trim,
    center.at=NULL,
    align.integers=c("dot","right","left"),
    topsep="",
    bottomsep="",
    sectionsep="",
    compact=TRUE,
    forLaTeX=FALSE,
    useDcolumn=TRUE,
    colspec=if (useDcolumn)
        paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="")
        else "r",
    LaTeXdec=".",
    ddigits=getOption("digits"),
    useBooktabs=TRUE,
    toprule=if(useBooktabs) "\\\\toprule" else "\\\\hline\\\\hline",
    midrule=if(useBooktabs) "\\\\midrule" else "\\\\hline",
    cmidrule=if(useBooktabs) "\\\\cmidrule" else "\\\\cline",
    bottomrule=if(useBooktabs) "\\\\bottomrule" else "\\\\hline\\\\hline",
    interaction.sep = if(forLaTeX) " $\\\\times$ " else " x ",
    center.summaries=FALSE,
    ...
    )

\method{print}{mtable}(x,trim=FALSE,center.at=getOption("OutDec"),
    colsep=" ",
    topsep="=",bottomsep="=",sectionsep="-",...)

\method{toLatex}{mtable}(object,...)

write.mtable(object,file="",...)
}
\arguments{
     \item{...}{as argument to \code{mtable}: several model objects, e.g. of class \code{\link{lm}}; as argument to \code{print.mtable},
     \code{toLatex.mtable}, \code{write.mtable}: further arguments
     passed to \code{format.mtable}; as argument to \code{format.mtable}:
     further arguments passed to \code{format.default};
     as argument to \code{relabel.mtable}: further arguments
     passed to \code{\link{dimrename}}.
     }
     \item{coef.style}{a character string which specifies the style of
        coefficient values, whether standard errors, Wald/t-statistics,
         or significance levels are reported, etc. See \code{\link{coef.style}}.}
     \item{summary.stats}{if \code{FALSE}, no summary statistics
        are repored. If \code{TRUE}, all summary statistics
        produced by \code{getSummary} are reported.
        This argument may also contain a character vector with
        the names of the summary statistics to report}
     \item{factor.style}{a character string that specifies the style in
        which factor contrasts are labled. See \code{\link{factor.style}}.}
     \item{getSummary}{a function that computes model-related statistics that
        appear in the table. See \code{\link{getSummary}}.}
     \item{float.style}{default format for floating point numbers if
        no format is specified by \code{coef.style}; see \code{{float.style}}.
        }
     \item{digits}{number of significant digits if not specified by
        the template returned from \code{\link{getCoefTemplate}}
        \code{\link{getSummaryTemplate}}
        }
     \item{drop}{logical value; should redundant column headings
        dropped if only one model is given as argument?}
     \item{x, object}{an object of class \code{mtable}}
     \item{gsub, warn, fixed}{logical values, see \code{\link{relabel}}}
     \item{coef.title}{a character vector, the title for the reported coefficients.}
     \item{summary.title}{a character vector, the title for the reported model summaries.}
     \item{colsep}{a character string which seperates the columns in the output.}
     \item{rowsep}{a character string which seperates the rows in the output.}
     \item{trim}{should leading and trailing spaces be trimmed?}
     \item{trimleft}{should leading spaces be trimmed?}
     \item{trimright}{should trailing spaces be trimmed?}
     \item{center.at}{a character string on which resulting values are centered.
        Typically equal to ".". This is the default when \code{forLaTeX==TRUE}.
        If \code{NULL}, reported values are not centered.}
     \item{align.integers}{how to align integer values.}
     \item{topsep}{a character string that is recycled to a top rule.}
     \item{bottomsep}{a character string that is recycled to a bottom rule.}
     \item{sectionsep}{a character string that is recycled to seperate coefficients
        from summary statistics.}
     \item{compact}{logical value; if TRUE, entries in the table are
      not aligned in any way, giving the table the most compact form.}
     \item{forLaTeX}{should LaTeX code be produced?}
     \item{useDcolumn}{should the \code{dcolumn} LaTeX package be used?
          If true, you will have to include \code{\\usepackage{dcolumn}} into
          the preamble of your LaTeX document.}
     \item{colspec}{LaTeX table column format specifyer(s).}
     \item{LaTeXdec}{the decimal point in the final LaTeX output.}
     \item{ddigits}{alignment specification or digits after the decimal point.}
     \item{useBooktabs}{should the \code{booktabs} LaTeX package be used?
          If true, you will have to include \code{\\usepackage{booktabs}} into
          the preamble of your LaTeX document.}
     \item{toprule}{appearance of the top border of the LaTeX \code{tabular} environment.}
     \item{midrule}{how are coefficients and summary statistics
        separated in the LaTeX \code{tabular} environment.}
     \item{cmidrule}{appearance of rules under section headings.}
     \item{bottomrule}{appearance of the bottom border of the LaTeX \code{tabular} environment.}
     \item{interaction.sep}{a character string that separates factors that are involved
        in an interaction effect}
     \item{center.summaries}{logical value; if TRUE, summaries for
      each model are centered below the columns that correspond to
      the respective model coefficients. }
     \item{file}{a file where to write to; defaults to console output.}
}
\details{
  \code{mtable} constructs a table of estimates for regression-type models.
  \code{format.mtable} formats suitable for use with output or conversion functions
  such as \code{print.mtable}, \code{toLatex.mtable}, or \code{write.mtable}.
}
\value{
    A call to \code{mtable} results in an object that inherits from \code{mtable}
    with the following components:

    \item{coefficients}{an array that contains the model coefficients.}
    \item{summaries}{a matrix that contains the model summaries.}

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

# This produces output in tab-delimited format:
write.mtable(mtable123)

\dontrun{
# This produces output in tab-delimited format:
file123 <- "mtable123.txt"
write.mtable(mtable123,file=file123)
file.show(file123)
# The contents of this file can be pasted into Word
# and converted into a Word table.
}
\dontshow{toLatex(mtable123)}
\dontrun{texfile123 <- "mtable123.tex"
write.mtable(mtable123,forLaTeX=TRUE,file=texfile123)
file.show(texfile123)}

berkeley <- Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)

berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

mtable(berk0,summary.stats=c("Deviance","N"))
mtable(berk0,drop=FALSE,summary.stats=c("Deviance","N"))
mtable(berk1,summary.stats=c("Deviance","N"))
mtable(berk1,drop=FALSE,summary.stats=c("Deviance","N"))


mtable(berk0,berk1,berk2,summary.stats=c("Deviance","N"))

mtable(berk0,berk1,berk2,
          coef.style="horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="stat",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.se",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.se.horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="ci.p.horizontal",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="all",
          summary.stats=c("Deviance","AIC","N"))
mtable(berk0,berk1,berk2,
          coef.style="all.nostar",
          summary.stats=c("Deviance","AIC","N"))

mtable(by(berkeley,berkeley$Dept,function(x)glm(cbind(Admitted,Rejected)~Gender,
        data=x,family="binomial")),
      summary.stats=c("Likelihood-ratio","N"))


mtable(By(~Gender,glm(cbind(Admitted,Rejected)~Dept,family="binomial"),
        data=berkeley),
      summary.stats=c("Likelihood-ratio","N"))

berkfull <- glm(cbind(Admitted,Rejected)~Dept/Gender - 1,
                      data=berkeley,family="binomial")
relabel(mtable(berkfull),Dept="Department",gsub=TRUE)

}
\keyword{misc}
