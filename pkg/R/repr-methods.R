repr_html.memisc_mtable <- function(obj) {
    if(getOption("memisc.repr_html",FALSE))
        format_html(obj)
    else
        mtable_format_print(obj)
}

repr_html.ftable <- function(obj) {
    if(getOption("memisc.repr_htm",FALSE))
        format_html.ftable(obj)
    else
        format(obj)
}

repr_html.ftable_matrix <- function(obj) {
    if(getOption("memisc.repr_htm",FALSE))
        format_html.ftable_matrix(obj)
    else
        format.ftable_matrix(obj)
}

repr_html.html_elem <- function(obj) as.character.html_elem(obj)

repr_html.html_group <- function(obj) as.character.html_group(obj)

repr_latex.memisc_mtable <- function(obj) {

    if(getOption("memisc.repr_latex",FALSE)){
        res <- mtable_format_latex(obj,
                                   useDcolumn=getOption("memisc.repr_latex.dcolumn",FALSE))
        paste0(res,collapse="\n")
    }
    else
        mtable_format_print(obj)
}
