eval.by <- function(f,expr) UseMethod("eval.by")
eval.by.default <- function(f,expr){
  parent <- parent.frame()
  expr <- substitute(expr)
  vars <- all.vars(expr)
  vars <- data.frame(sapply(vars,get,parent))
  vars <- split(vars,f)
  res <- lapply(vars,function(v)eval(expr,v,parent))
  unsplit(res,f)
}
