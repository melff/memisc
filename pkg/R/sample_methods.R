setMethod("sample", "data.frame", function (x, size, replace = FALSE, prob = NULL){
    if (missing(size))
        size <- nrow(x)
    ii <- .Internal(sample(nrow(x), size, replace, prob))
    x[ii,,drop=FALSE]
})

setMethod("sample", "data.set", function (x, size, replace = FALSE, prob = NULL){
    if (missing(size))
        size <- nrow(x)
    ii <- .Internal(sample(nrow(x), size, replace, prob))
    x[ii,,drop=FALSE]
})

setMethod("sample", "importer", function (x, size, replace = FALSE, prob = NULL){
    if (missing(size))
        size <- nrow(x)
    ii <- .Internal(sample(nrow(x), size, replace, prob))
    x[ii,,drop=FALSE]
})

