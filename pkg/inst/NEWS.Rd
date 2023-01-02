\name{NEWS}
\title{\emph{memisc} News}
\encoding{UTF-8}

\section{Version 0.99}{
  \subsection{NEW FEATURES}{
  \itemize{
    \item A new object-oriented infrastructure for the creation of HTML code
          is used in \code{format_html()} methods. This infrastructure is exposed 
          by the \code{html()} function.
    \item Support for with model groups in \code{mtable()}. \code{c.mtable()} now creates 
          groups of models, if arguments are tagged.
    \item Flattened contingency tables (\code{ftable()}s as they are created
          by the eponymous function in the \code{stats()} package) can now be
          combined into \code{ftable_matrix()} objects. This can be done by
          using \code{rbind()} or \code{cbind()}.
    \item There is now an object class for survey items containing dates (without
          times), called \code{"Date.item"}.
    \item Support for including sandwich estimates of sampling variances and
          standard errors into the output of \code{summary()} and \code{mtable()},
          by the new generic functions \code{withVCov()} and
          \code{withSE()}.
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
          selected for each object or object class (via calls to \code{options()})
          separately.
    \item It is now possible to compress the output concerning control
          variables in \code{mtable()}.
    \item Support for HTML and LaTeX output in Jupyter notebooks is added to objects
          created by \code{mtable()} and \code{ftable()} etc.
    \item The \code{toLatex()} method for "ftable" objects gains a
          \code{fold.leaders} option (with default value \code{FALSE})
          which allows the row labels (leaders) to remain in a single column.
    \item A function \code{codeplan()}  creates a data frame
          describing the structure of an "importer", "data.set" or "item"
          object. It is possible to copy this so described structure 
          from one "data.set" object to another or to a data frame.
    \item New \code{$} and \code{[[} operators for "importer" objects
          allow to create codebooks for single items/variables in 
          imported data files.
    \item A \code{duplicated_labels()} function allows to show and
          describe duplicated labels and a \code{deduplicate_labels()}
          function allows to get rid of such duplicates.
    \item New operators \code{\%#\%}, \code{\%##\%}, and \code{\%@\%} to
          manipulate annotations and other attributes.
    \item A \code{List()} function adds names to its elements by deparsing
          arguments in the same way as \code{data.frame()} does.
    \item A new function \code{Groups()} allows to split a data frame or a
          "data.set" into group based on factors in a more convenient
          way. There are methods of \code{with()} and \code{within()} to deal with
          resulting objects of class "grouped.data". For example, the
          \code{within()} method allows to substract group means from the
          observations within groups. \code{withinGroups()} allows to split a
          data frame or "data.set" objects into groups, make within-group
          computations and recombine the groups into the order of the original
          data frame or "data.set" object.
    \item A new function \code{Reshape()} simplifies the syntax to reshape data frames
          and "data.set" objects from wide into long or from long into
          wide format.
    \item 'tibbles', including those created with the \emph{haven}
          package can be translated into "data.set" objects without loss
          of information. Also "data.set" objects can be translated into
          'tibbles' with minimal loss of information.
    \item An extendable function \code{view()} allows to use the
          \code{View()} facilities provided by graphical user interfaces (in
          particular RStudio) with objects not originally supported by these
          user interfaces. In addition, \code{view()} methods for
          "codeplan", "decriptions", "data.set", and "inporter" are
          provided, which allow to conviently inspect the contents of
          these objects in RStudio.
    \item An "as.data.table" for coercing "data.set" objects directly
          into "data.table" objects.
    \item It is now possible to specify the measurement level for a set
          of variables in a "data.set" objects, either by using the
          assignment operator with \code{measurement()} or by using
          the new function \code{set_measurement()}.
    \item There are convenience wrappers such as \code{Mean()} etc. for
          \code{mean()} etc. that have the default setting \code{na.rm=TRUE} 
          instead of \code{na.rm=FALSE}.
    \item A new \code{deduplicate_labels()} function allows to deal with
          duplicate labels (where several codes have the same label)
    \item It is now possible to create codebooks for weighted data.
    \item The function \code{trim_labels()} allows to trim codes from
          value labels.
    \item The function \code{reverse()} allows to reorder the
          codes of a survey item in reverse order.
    \item The generic function \code{Means()} allows to conveniently obtain
          group means, optionally with standard errors and/or confidence
          intervals.
    \item The colon operator (\code{:}) can be used to refer to ranges of
          variables in \code{foreach()}
    \item Code plans (objects in class "codeplan") can now be exported
          to and imported from YAML and JSON files.
    \item A new generic function \code{format_md()}
          (contributed by Mael Astrud-Le Souder) allows to format R
          objects in Markdown. Currently, methods for codebooks (and entries
          in codebooks) are implemented.
    \item A new generic function \code{coarsen()} allows to coarsen
          numeric vectors into factors, based on a given number of
          categories.
    \item A new generic function \code{measurement_autolevel()} 
          allows to automatically select the appropriate measurement
          level for survey items.
    \item A new operator \code{\%if\%} allows to assign values to a variable
          for observations that satisfy a condition.      
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
      \item The \code{toLatex()} method for "ftable" objects has a new
            option \code{fold.leaders}. 
      \item \code{spss.system.file()} now translates numeric variables with any SPSS
            date format into a "datetime.item"
      \item The function \code{List()} adds names to the elements of the
            resulting list in a way similar to how \code{data.frame()}
            adds names to the columns of a data frame. 
      \item \code{Stata.file()} now handles files in format rev. 117 and
            later as they are created by Stata version later than 13.
      \item User definded missing values are now reported in separate
            tables in entries created by \code{codebook()} even if these
            entries refer to items with measurement level "interval" or
            "ratio".
      \item If the annotation or the labels of a non-item is set to NULL
            this no longer causes an error.
      \item Changing varible names to lowercase while importing data
            sets with \code{Stata.file()}, \code{spss.portable.file()}, and
            \code{spss.system.file()} is now optional.
      \item Importer methods \code{Stata.file()}, \code{spss.portable.file()}, and
            \code{spss.system.file()} now have optional arguments that
            allow to deal with variable labels or value labels in
            non-native encoding (e.g. \code{CP1252} on a \code{utf-8}
            platform).
      \item A function \code{spss.file()} acts as a common interface to
            \code{spss.portable.file()} and \code{spss.system.file()}.
      \item The function \code{head()} and \code{tail()} now work with
            "data.set" and "importer" objects in the same sensible way as they
            do with data frames.
      \item The function \code{recode()} behaves more coherently:  If a labelled
            vector is the result of 'recode' it gets the measurement level
            "nominal". Factor levels explictly created first come first in the order of
            factor levels.
      \item The function \code{spss.system.file()} now handles buggy SPSS
            system files that lack information about the number of
            variables in their header. (These files are typically
            created by the library ReadStat, used e.g. by the R package
            'haven'.)
      \item SPSS syntax files are now converted to the encoding of
            the host system if they have a different one. By default,
            the original encoding is assumed to be Codepage 1252 (extended
            Latin-1).
      \item \code{codebook()}, \code{codeplan()}, \code{labels()},
            \code{value.filter}, and related functions return \code{NULL} 
            for \code{NULL} arguments.
      \item \code{codeplan()} also works with indiviual survey items and
            can set to \code{NULL}, which means that all memisc-specific
            information is removed from the data. 
      \item \code{codebook()} works also with data frames (or "tibbles")
            imported with the \emph{haven} package.
      \item \code{codebook()} now makes use of the "label" attribute of variables if the
            attribute is present.
      \item \code{with(Groups())}, \code{withGroups()},
            \code{within(Groups())}, \code{withinGroups()},
            \code{Aggregate()}, and \code{genTable()} are considerably
            faster now. They can also make use of certain automatic
            variables such as \code{n_}, \code{i_} that contain group
            sizes and group indices.
      \item \code{relabel()}, \code{rename()}, and \code{dimrename()},
            do no longer require their arguments to be enclosed in
            quotation marks.
      \item Operators '$', '[', and  '[[' can now be appied to codebook objects
            to get a codebook of a subset of the varaibles.
      \item \code{spss.system.file()} now uses information contained in SPSS
            files (if available) to determine the measurement level
            of the improrted variables.
      \item \code{spss.system.file()} uses information about the character set
            encoding if available in the file to translate variable labels and
            value labels into the coding of the machine on which \emph{R} is
            being run.
      \item \code{spss.system.file()} also (optionally) uses information
            about the intended measurement level fo variables in the file.
     \item  \code{as.item()} now drops non-unique labelled values when applied to a
            "labelled", "haven_labelled", or "haven_labelled_spss" object.
     \item \code{spss.system.file()} no takes into account metadata about
            measurement levels ("nominal", "ordinal", or "scale") to set
            the \code{measurement()} attributes of the items in
            the resulting \code{"importer"} and \code{"data.set"}
            objects.
     \item \code{mtable()} now handles objects of class "clmm" (from package "ordinal")
           and the handling of objects of class "merMod" (from package "lme4")
           is more consistent with those of class "glm" (e.g. the number of
           observations is shown).
     \item Variance component estimates of "merMod" and "clmm" objects are reported
           as distinct statistics.
     \item \code{recode()} has a new optional argument \code{code=}. If
           \code{TRUE}, existing codes (and labels) are retained.
     \item \code{recode()} now allows to recode factors into numeric vectors.
     \item If the change in codes done by \code{recode()} merely
           reorders codes, labels are reordered accordingly, unless
           labels are explicitly given.
     \item \code{subset()} is S3-generic again, as this allows for lazy
           evaluation of its arguments.
    \item \code{cases()} handles \code{NAs} more sensibly - if a
          case condition is \code{TRUE} this leads to a non-\code{NA} 
          result even if other conditions evaluate to \code{FALSE}, if
          \code{cases()} is called with \code{na.rm=TRUE} (the
          default). 
    \item The result of \code{subset} and of the bracket-operator (\code{[]})
          applied to importer objects has row names that indicate the rows
          selected from the full data.
    \item A method of \code{format} for data set objects is added.
    \item The row names of subsets fo importer objects reflect the row numbers in
          the original data.
    \item \code{collect.data.frame} and \code{collect.data.set} gain a 
          \code{use_last} and a \code{detailed_warnings} option
          to improve handling of variables with different attributes
          in different objects being collected.
    \item \code{spss.system.file()}, \code{spss.portable.file()}, and \code{Stata.file()} get
          an optional \code{negative2missing} argument.
    \item \code{recode()} keeps \code{NA}s as \code{NA}s when an
          \code{otherwise} argument is given and \code{NA}s are not
          recoded explicitly.
    \item \code{codebook()} now fully supports logical vectors.      
    }
 }
  \subsection{BUGFIXES}{
  \itemize{
    \item \code{str} and \code{ls.str} are imported from the 
          \code{utils} package to prevent a NOTE in R CMD check
    \item HTML tables and lists are no loger wrapped in HTML paragraphs
          in \code{format_html.CodebookEntry}.
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
    \item Some PROTECTION issus in the C-source flagged by Tomas
          Kalibera's \code{rchk} utility are fixed.
    \item If "data.set" objects are combined and succeeding objects contain
          "items" not contained in the preceding ones, the result now will
          still be a valid "data.set" object.
    \item \code{seekData} etc. no longer try to recreate external
          pointers in order to avoid segmentation faults. Also the deletion of
          empty pointers is avoided for the same reason.
    \item \code{as.data.set} works for "tibbles" also when method dispatch
          via class inheritance does not work.
    \item \code{codebook()} now handles character variables in SPSS
          system files correclty.
    \item \code{codebook()} uses the appropriate logical operator in checking
          for missings.
    }
  }
  \subsection{USER-VISIBLE CHANGES}{
   \itemize{
     \item All vignettes are now using \emph{knitr}.
     \item HTML output uses unicode characters by default instead of
     amersand-escapes to enhance compatibility with \emph{pandoc}.
     \item \code{codebook()} no longer shows the skewness and kurtosis of
           numeric variables to save output space.    
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
