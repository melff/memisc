format_md.codebook <- function(x, unlist = TRUE, collapse = TRUE, ...) {
  output <- mapply(format_md, x = x@.Data, name = names(x),
                   MoreArgs = list(...))
  names(output) <- names(x)
  if (unlist) output <- unlist(output)
  if (collapse && is.list(output)) {
    output <- lapply(output, paste, collapse = "\n\n")
    output <- lapply(output, gsub, 
                     pattern = "|\n\n", replacement = "|\n", fixed = TRUE)
  }
  if (collapse && !is.list(output)) {
    output <- paste(output, collapse = "\n\n")
    output <- gsub(output,
                   pattern = "|\n\n", replacement = "|\n", fixed = TRUE)
  }
  output
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
  tab <- cbind(rownames(tab), tab)
  rownames(tab) <- NULL
  colnames(tab)[1] <- "Values and labels"
  knit_counts <- c(knitr::kable(tab))
}
