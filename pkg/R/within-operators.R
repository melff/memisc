Within <- function(data, expr, ...){
    if(inherits(data,"data.frame"))
        UseMethod("Within")
    else UseMethod("within")
}

Within.data.frame <- function (data, expr, ...) 
{
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l <- rev(as.list(e, all.names = TRUE))
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    nl <- names(l)
    del <- setdiff(names(data), nl)
    data[nl] <- l
    data[del] <- NULL
    data
}

"%$$%" <- function(data,expr){
    # parf <- parent.frame()
    # nm <- deparse(x)
    # res <- within(x,expr)
    # res
    res <- if(inherits(data,"data.frame"))
               eval.parent(substitute(Within(data,expr)))
           else
               eval.parent(substitute(within(data,expr))) 
    nm <- deparse(substitute(data))
    assign(nm,res,envir=parent.frame())
}

"%$%" <- function(data,expr){
    eval.parent(substitute(with(data,expr)))
}
