\name{NEWS}
\title{memisc News}
\encoding{UTF-8}
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
      and \code{codebooks}, can be exported int HTML using \code{format_html}
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