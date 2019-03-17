\name{NEWS}
\title{\emph{memisc} News}
\encoding{UTF-8}

\section{Version 0.99}{
  \subsection{NEW FEATURES}{
  \itemize{
    \item A new object-oriented infrastructure for the creation of HTML code
          is used in \code{format_html} methods. This infrastructure is exposed 
          by the \code{html} function.
    \item Support for with model groups in \code{mtable}. \code{c.mtable} now creates 
          groups of models, if arguments are tagged.
    \item Flattened contingency tables (\code{ftable}s as they are created
          by the eponymous function in the \code{stats} package) can now be
          combined into \code{ftable_matrix} objects. This can be done by
          using \code{rbind()} or \code{cbind()}.
    \item There is now an object class for survey items containing dates (without
          times), called \code{"Date.item"}.
    \item Support for including sandwich estimates of sampling variances and
          standard errors into the output of \code{summary} and \code{mtable},
          by the new generic functions \code{withVCov} and
          \code{withSE}.
    \item Support for different parameter sections is added to
          \code{mtable}. This is intended to allow output of mixed
          effects  models to distinguish between ("fixed effects")
          coefficients and variance parameters.
    \item Objects created by \code{mtable()} also can have several
          header lines. Facilities to add additional header lines will
          be added soon.
    \item Optionally, \code{mtable()} shows the left-hand sides of
          model equations. This can be controlled by the optional
          argument \code{show.eqnames} and by the global option
          \code{"mtable.show.eqnames"}.
    \item Output of \code{mtable()} objects also include, if applicable,
          a note that explains the "significance stars" for p-values.
    \item Summary statistics reported by \code{mtable()} can now be
          selected for each object or object class (via calls to \code{opitons})
          separately.
    \item Support for HTML and LaTeX output in Jupyter notebooks is added to objects
          created by \code{mtable} and \code{ftable} etc.
    \item A function \code{codeplan()}  creates a data frame
          describing the structure of an "importer" or "data.set"
          object. It is possible to copy this so described structure 
          from one "data.set" object to another or to a data frame.
    \item New \code{$} and \code{[[} operators for "importer" objects
          allow to create codebooks for single items/variables in 
          imported data files.
    \item New operators \code{\%#\%}, \code{\%##\%}, and \code{\%@\%} to
          manipulate annotations and other attributes.
    }
  }
  \subsection{IMPROVEMENTS}{
    \itemize{
      \item Subset methods for importer objects are much more memory
            efficient and now can handle files of size larger than 1GB.
      \item \code{useDcolumn} and \code{useBooktabs} arguments of
            \code{toLatex()} methods now have global options as defaults
      \item \code{toLatex()} methods optionally escape dollar, subscript and
            superscript symbols. This can be set either by an explicit
            (new) argument \code{toLatex.escape.tex} or by a global option
            with the same name.
      \item \code{spss.system.file} now translates numeric variables with any SPSS
            date format into a "datetime.item"
    }
  }
  \subsection{BUGFIXES}{
  \itemize{
    \item \code{str} and \code{ls.str} are imported from the 
          \code{utils} package to prevent a NOTE in R CMD check
    \item HTML tables and lists are no loger wrapped in HTML paragraphs in \code{format_html.CodebookEntry}.
    \item \code{show} and \code{codebookEntry} methods for the
    "datetime.item" now work asexpected. 
    \item \code{cases} handles \code{NA}s more gracefully
    \item \code{toLatex.ftable} output has been improved: No attempt at
          showing non-existent variable names, better application of
          \code{extracolsep}.
    \item Duplicate value labels now produce an error if item object is
          coerced into a factor.
    \item A bug concerning missing values in SPSS files is fixed.
    \item Headlines in vignettes are now coherent.
    \item \code{mtable} with empty summary sections can be created
    (again).
    \item Objects returned by \code{mtable} return objects with class
          "memisc_mtable" to avoid name clash with objects created by the
          \code{model.table} in package "stats".
    \item Calls to PROTECTION are added to the C-source to prevent protection
    errors.
    \item \code{toLatex()} now handles matrices in data frames.
    \item \code{spss.portable.file()} now handles files with weighting
    variables and empty variable labels.
    \item \code{spss.fixed.file()} now handles files with lines that are
    longer than the number of columns specified in the columns
    definition file.
    \item \code{spss.system.file()} now correctly imports value labels
           of string variables.
    }
  }
  \subsection{USER-VISIBLE CHANGES}{
   \itemize{
     \item All vignettes are now using \emph{knitr}.
     \item HTML output uses unicode characters by default instead of
     amersand-escapes to enhance compatibility with \emph{pandoc}.
   }
  }
  \subsection{DEFUNCT}{
    \itemize{
      \item The function \code{UnZip} has been removed from the package. \code{unzip} in conjunction with
      \code{system.file} does the same job, as can be seen in the example for \code{spss.portable.file}.
    }
  }
}


\section{Version 0.98}{
  \subsection{NEW FEATURES}{
  \itemize{
      \item Support for exporting results of various functions into HTML format is now
      supported by the function \code{format_html}. 
      This should make it easier to import them into HTML or word-processing
      documents (that support importing HTML). A preview of the HTML is made available
      by the new (generic) function \code{show_html}.
      
      In particular, results of the functions \code{mtable} (i.e. tables of
      model estimates), \code{ftable} (i.e. flattened contingency tables etc.),
      and \code{codebooks}, can be exported int. HTML using \code{format_html}.
      Also data frames can be exported into HTML.
      
      \item A function \code{dsView} is added, which allows a display of \code{data.set}
      objects similar as \code{View} displays data frames.
      
      \item \code{mtable} now handles multi-equation models better, in particular 
      if the model objects supplied as arguments vary in the number and/or names
      of the equations. There is also a new option to place confidence intervals 
      to the right of coefficient estimates. Further \code{mtable} gains
      the following optional aguments:
      \itemize{
        \item \code{show.baselevel}, which
              allows to suppress the display of baseline categories of dummy variables,
              when dummy variable coefficients are displayed
        \item \code{sdigits}, to specify the number of digits of summary statistics.
        \item \code{gs.options}, to pass optional arguments to \code{getSummary}, allowin
        for more flexibility in creating tables.
      }
      One can now use a \code{summaryTemplate}
      generic function for formatting model summaries, in addition to set the template by
      \code{setSummaryTemplate}.  
      Finally, parts of "mtables" can be extracted using the \code{[} operator as
      with matrices, and "mtables" can now also be concatenated.
      
      \item There is now an object class for survey items containing dates and times, called
      \code{"datetime.item"}
      
      \item There is a new function \code{wild.codes} to check wild codes (i.e. 
      unlabelled codes of an otherwise labelled item.)
      
      \item \code{codebook} now supports data frames, factors, and numeric vectors.
      
      \item A \code{toLatex} method exists now for \code{data.set} objects, data frames and other objects.
      
      \item A new \code{percentages} function is added to allow easy creation of tables of percentages.
    }
  }
  \subsection{BUGFIXES}{
  \itemize{
      \item \code{spss.fixed.file} is now able to handle labelled strings and 
       \code{value labels} and \code{missing values} statements. 
      \item Internal C-code used by \code{spss.fixed.file} no longer assumed that
      arguments are copied -- some strange behaviour of objects created by \code{spss.fixed.file}
      is now corrected.
      \item Description of items in external data sources is more complete now - the same
      information as for items in internal \code{data.sets}.
      \item \code{applyTemplate} now returns empty strings for undefined quantities.
      \item \code{collect} method for \code{data.sets} now works as expected.
      \item \code{spss.fixed.file} now checks whether there are undefined variables in \code{varlab.file} etc.
      
      \item \code{Stata.file} now can import Stata 9 and Stata 10 files.
    }
  }
  \subsection{USER-VISIBLE CHANGES}{
   \itemize{
     \item Argument \code{drop} no longer used by function \code{mtable}.
     \item Format of file produced by \code{write.mtable} can now be
     specified using a \code{format=} argument. But \code{forLaTeX=TRUE}
     still can be used to get LaTeX files.
   }
  }
  \subsection{DEFUNCT}{
    \itemize{
      \item The functions \code{Termplot}, \code{Simulate}, and \code{panel.errbars}
      are defunct. Graphics similar to those built with \code{panel.errbars}
      can be created with facilities provided by the package "mplot", which 
      is currently available on GitHub.
    }
  }
}
\section{Version 0.97 }{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{spss.system.file} and \code{spss.portable.file} gain a \code{tolower=} argument 
      that defaults to \code{TRUE}, which 
      allows to change annoying all-upper-case variable names to lower case
      \item New generic function \code{Iconv()} that allows to change the character enconding of variable
      descriptions and value labels. It has methods for \code{"data.set"}, \code{"importer"}, \code{"item"},
      \code{"annotation"}, and \code{"value.label"} objects.
      \item There is now a method of \code{as.character()} for \code{"codebook"} objects and a convenience
      function \code{Write()} with methods for \code{"codebook"} and \code{"description"} to make it more
      convenient to direct the output of \code{codebook()} and \code{description()} into text files.
      \item A method for \code{"merMod"} objects of the \code{getSummary()} generic function. 
      \code{mtable()} now should be able (again) to handle estimation results produced by \code{lmer()} and \code{glmer()} from 
      package 'lme4'.
      \item \code{recode()} handles character vectors in a more convenient way: They are converted
    into factors with sorted unique values (after recoding) as levels.
    }
  }
  \subsection{USER-VISIBLE CHANGES}{
   \itemize{
     \item \code{getSummary.expCoef} is renamed into \code{getSummary_expCoef}.
   }
  }
  \subsection{DEFUNCT}{
    \itemize{
      \item S3 method \code{aggregate.formula} has been removed from the package to avoid
      clash with method of the same name in the \code{base} package. The function \code{Aggregate}
      can be used instead.
      \item Removed \code{include}, \code{uninclude}, and \code{detach.sources} as these are flagged
      as modifying the global namespace.
    }
  }
}
