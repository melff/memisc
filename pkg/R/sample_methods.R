sample.data.frame <- function (x, size, replace = FALSE, prob = NULL, ...){
    if (missing(size))
        size <- nrow(x)
    x[.Internal(sample(nrow(x), size, replace, prob)),,drop=FALSE]
}

sample.data.set <- function (x, size, replace = FALSE, prob = NULL, ...){
    if (missing(size))
        size <- nrow(x)
    x[.Internal(sample(nrow(x), size, replace, prob)),,drop=FALSE]
}

sample.importer <- function (x, size, replace = FALSE, prob = NULL, ...){
    if (missing(size))
        size <- nrow(x)
    x[.Internal(sample(nrow(x), size, replace, prob)),,drop=FALSE]
}
