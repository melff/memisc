
retain <- function (..., list = character(0), envir = parent.frame(),force=FALSE)
{
    dots <- match.call(expand.dots = FALSE)$...
    if(environmentName(envir)==environmentName(globalenv()) && !force) {
      warning("retain will remove objects from global environment only if force=TRUE")
      return(invisible(NULL))
    }
    if(!length(dots)) stop("at least one object has to be retained")
    if (!all(sapply(dots, function(x) is.symbol(x) ||
        is.character(x))))
        stop("... must contain names or character strings")
    names <- sapply(dots, as.character)
    if (length(names) == 0)
        names <- character(0)
    list <- .Primitive("c")(list, names)
    obs <- ls(envir=envir)
    if(!all(list %in% obs)) stop("cannot retain undefined objects")
    to.remove <- obs[!(obs %in% list)]
    remove(list=to.remove, envir=envir, inherits=FALSE)
}
