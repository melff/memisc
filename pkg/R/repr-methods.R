repr_html.mtable <- function(obj) format_html(obj)

repr_html.ftable <- function(obj) format_html.ftable(obj)

repr_html.ftable_matrix <- function(obj) format_html.ftable_matrix(obj)

repr_html.html_elem <- function(obj) as.character.html_elem(obj)

repr_html.html_group <- function(obj) as.character.html_group(obj)
