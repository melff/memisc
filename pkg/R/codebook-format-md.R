format_md.codebook <- function(x, unlist = TRUE, collapse = TRUE, ...) {
  out <- mapply(format_md, x = x@.Data, name = names(x),
                   MoreArgs = list(...))
  names(out) <- names(x)
  if (unlist) out <- unlist(out)
  if (collapse && is.list(out)) out <- lapply(out, paste, collapse = "  \n")
  if (collapse && !is.list(out)) out <- paste(out, collapse = "  \n")
  out
}

format_md.codebookEntry <- function(x, name = "", add_lines = TRUE, ...) {
  blank_line <- "\n"
  horizontal_line <- if (add_lines) "\n---\n" else NULL
  
  annot <- x@annotation
  stats <- x@stats
  
  title <- paste0("`", name, "`", " --- ", "'", annot["description"], "'")
  wording <- if (!is.na(annot["wording"])) paste0("\"", annot["wording"], "\"") else NULL
  knit_spec <- knit_array(x@spec)
  knit_tab <- if (!is.null(stats$tab)) knit_tab(stats$tab) else NULL
  knit_descr <- if (!is.null(stats$descr)) knit_array(stats$descr) else NULL
  remark <- paste0("Remark: ", annot["Remark"])
  
  output <- c(horizontal_line,
              title,
              wording,
              horizontal_line,
              knit_spec,
              blank_line,
              knit_tab,
              knit_descr,
              blank_line,
              remark,
              blank_line
  )
  
  output <- output[!sapply(output, is.null)]
}

knit_array <- function (x) {
  if (!is.matrix(x)) x <- as.matrix(x)
  df <- data.frame(names = rownames(x), content = unname(x))
  colnames(df) <- NULL
  knit_result <- c(knitr::kable(df, format = "simple"))
  knit_result <- knit_result[-c(1, length(knit_result))]
  knit_result <- c(knit_result)
}

knit_tab <- function (x) {
  tab <- data.frame(x[,,1])
  row_names <- rownames(tab)
  values <- regmatches(row_names, regexpr(row_names, pattern = "^[ ]*[0-9]+"))
  values <- as.numeric(values)
  missing_value <- grepl(rownames(tab), pattern = " M '")
  missing_value <- ifelse(missing_value, "M", "")
  labels <- regmatches(row_names, regexpr(row_names, pattern = "'.*'"))
  tab <- cbind(values, missing_value, labels, tab)
  rownames(tab) <- NULL
  colnames(tab) <- c("", "", "Values and labels", "N", "Valid", "Total")
  knit_counts <- c(knitr::kable(tab))
}