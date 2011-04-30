By <- function(formula,expr,data=parent.frame()) UseMethod("By",data)

By.default <- function(formula,expr,data=parent.frame()){
  m <- match.call()
  parent <- parent.frame()
  expr <- m$expr
  formula <- m$formula
  if(is.environment(data)){
    allVars <- union(all.vars(formula),all.vars(expr))
    data <- lapply(allVars,function(v)get(v,envir=data))
    names(data) <- allVars
  }
  data <- as.data.frame(data)
  fun <- function(x) eval(expr,data[x,],parent)
  factors <- all.vars(formula)
  factors <- data[factors]
  nd <- nrow(data)
  ans <- eval(substitute(tapply(1:nd, factors, fun)), data)
  ans.is.nonnull <- !sapply(ans,is.null)
  if(!all(ans.is.nonnull)){
    ans <- if(is.matrix(ans)) ans[,ans.is.nonnull]
           else ans[ans.is.nonnull]
  }
  attr(ans, "call") <- match.call()
  class(ans) <- "by"
  ans
}

By.data.frame <- function(formula,expr,data){
  m <- match.call()
  parent <- parent.frame()
  expr <- m$expr
  formula <- m$formula
  if(is.environment(data)){
    allVars <- union(all.vars(formula),all.vars(expr))
    data <- lapply(allVars,function(v)get(v,envir=data))
    names(data) <- allVars
  }
  fun <- function(x) {
    xdata <- data[x,,drop=FALSE]
    res <- eval(expr,xdata,parent)
    if(is.list(res) && length(res[["data"]]) && is.environment(res[["data"]]))
      res[["data"]] <- xdata
    res
    }
  factors <- all.vars(formula)
  factors <- data[factors]
  nd <- nrow(data)
  ans <- eval(substitute(tapply(1:nd, factors, fun)), data)
  ans.is.nonnull <- !sapply(ans,is.null)
  if(!all(ans.is.nonnull)){
    ans <- if(is.matrix(ans)) ans[,ans.is.nonnull]
           else ans[ans.is.nonnull]
  }
  attr(ans, "call") <- match.call()
  class(ans) <- "by"
  ans
}
