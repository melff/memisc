repr_html.memisc_mtable <- function(obj) {
    if(getOption("memisc.repr_html",FALSE))
        format_html(obj)
    else {
        fmt <- mtable_format_print(obj)
        as.character(html_pre(fmt))
    }
}

repr_html.ftable <- function(obj) {
    if(getOption("memisc.repr_html",FALSE))
        format_html.ftable(obj)
    else {
        fmt <- format(obj)
        as.character(html_pre(fmt))
    }
}

repr_html.ftable_matrix <- function(obj) {
    if(getOption("memisc.repr_html",FALSE))
        format_html.ftable_matrix(obj)
    else {
        fmt <- format.ftable_matrix(obj)
        as.character(html_pre(fmt))
    }
}

repr_html.html_elem <- function(obj) as.character.html_elem(obj)

repr_html.html_group <- function(obj) as.character.html_group(obj)

repr_latex.memisc_mtable <- function(obj) {

    if(getOption("memisc.repr_latex",FALSE)){
        res <- mtable_format_latex(obj,
                                   useDcolumn=getOption("memisc.repr_latex.dcolumn",FALSE))
        paste0(res,collapse="\n")
    }
    else NULL
}
