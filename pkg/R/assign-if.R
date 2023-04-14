"%if%" <- function(expr,condition){
    m <- match.call()
    expr <- m$expr
    expr.ok <- !is.symbol(expr)
    expr.ok <- expr.ok && as.character(expr[[1]]) == "("
    if(expr.ok){
        expr <- expr[[2]]
        expr.ok <- expr.ok && as.character(expr[[1]]) == "<-"
    }
    if(!expr.ok) stop(sprintf("'%s' is not an assignment operation",deparse(m$expr)))   
    if(!is.logical(condition)) stop(sprintf("'%s' is not a logical condition",
                                            deparse(m$condition)))
    lhs <- expr[[2]]
    rhs_val <- eval(expr[[3]],envir=parent.frame())
    lhs_val <- if(exists(lhs,envir=parent.frame())) eval(lhs,envir=parent.frame())
               else rep(NA,length(condition))
    if(length(lhs_val) > 1 && length(rhs_val) > 1 &&
       length(rhs_val) != sum(condition) &&
       length(rhs_val) != length(lhs_val))
        warning("Non-matching lengths in assignment")
    lhs_val[] <- ifelse(condition,rhs_val,lhs_val)
    assign(as.character(lhs),value=lhs_val,envir=parent.frame())
}
