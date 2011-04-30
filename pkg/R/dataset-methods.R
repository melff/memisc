setValidity("named.list",function(object){
  if(all(is.na(object@names))) "list is unnamed"
  else if(length(object@.Data)!=length(object@names[nzchar(object@names)])){
    znames <- which(!nzchar(object@names))
    paste(
      if(length(znames) == 1) "element" else "elements",
      paste(znames,collapse=", "),
      if(length(znames) == 1) "is" else "are",
      "unnamed"
      )
    }
  else if(length(unique(object@names)) != length(object@names)) paste(
    "list has duplicate names:",
    paste(dQuote(object@names[duplicated(object@names)]),collapse=", ")
    )
  else TRUE
})

setValidity("data.set",function(object){
  isItemVector <- sapply(object,is,"item.vector")
  if(!all(isItemVector)) {
    wrong.els <- object[!isItemVector]
    wrong.classes <- sapply(wrong.els,class)
    wrong.names <- object@names[!isItemVector]
    paste(
      "object has elements of wrong class:",
      paste(
        paste("class(",wrong.names,") = ",wrong.classes,sep=""),
        collapse=", "
      )
    )
  }
  else if(any(length(object@row.names) != sapply(object,length))){
    wrong.els <- object[!isItemVector]
    wrong.names <- object@names[!isItemVector]
    wront.lengths <- sapply(object,length)
    paste(
    if(length(which(wrong.length)) > 1) "elements have" else "element has",
    "wrong length: ",
      paste(
        paste("class(",wrong.names,") = ",wrong.classes,sep=""),
        collapse=", "
      ),
    "where",
    length(object@row.names),
    "is required"
    )
  }
  else TRUE
})

setMethod("initialize","named.list",function(.Object,...){
  args <- list(...)
  if(is.list(args[[1]])) args <- unclass(args[[1]])
  .Object@.Data <- unname(args)
  .Object@names <- as.character(names(args))
  if(validObject(.Object)) .Object
})

setMethod("initialize","item.list",function(.Object,...){
  args <- list(...)
  if(is.list(args[[1]])) args <- unclass(args[[1]])
  .Object@.Data <- unname(lapply(args,as.item))
  .Object@names <- as.character(names(args))
  if(validObject(.Object)) .Object
})


# setMethod("initialize","data.set",function(.Object,...){
#   args <- list(...)
#   if(is.list(args[[1]])) args <- unclass(args[[1]])
#   res <- new("item.list",args)
#   browser()
#   class(res) <- class(.Object)
#   rn <- attr(args,"row.names")
#   if(is.null(rn)) rn <- seq_len(length(args[[1]]))
#   res@row_names <- rn
#   if(validObject(res)) res
# })

setMethod("show","named.list",function(object)
  print.default(unclass(object))
)

setLength <- function(x,n){
  tmp <- unname(x)
  length(x) <- n
  x[] <- tmp
  attributes(x) <- attributes(tmp)
  x
}

setMethod("initialize","data.set",function(.Object,...,row.names=NULL,document=character()){
  cl <- class(.Object)
  .Object <- new("item.list",...)
  nr <- max(sapply(.Object,length))
  .Object <- lapply(.Object,setLength,n=nr)
  .Object <- lapply(.Object,as.item)
  class(.Object) <- cl
  if (is.null(row.names))
      row.names <- seq_len(nr)
  else {
      if (is.object(row.names) || !is.integer(row.names))
          row.names <- as.character(row.names)
      if (any(is.na(row.names)))
          stop("row names contain missing values")
      if (any(duplicated(row.names)))
          stop("duplicate row.names: ", paste(unique(row.names[duplicated(row.names)]),
              collapse = ", "))
  }
  .Object@row_names <- row.names
  .Object@document <- document
  asS4(.Object)
})

# dim.data.set <- dim.data.frame
setMethod("dim","data.set",function(x)
  c( length(x@row_names),
     length(x@.Data)
  )
)

setMethod("row.names","data.set",function(x){
  x@row_names
})

setReplaceMethod("row.names","data.set",function(x,value){
  nr <- length(x@.Data[[1]])
  if(is.null(value)){
    value <- seq_len(nr)
  }
  else if(length(value) != nr)
    stop("invalid 'row.names' given for data set")
  x@row_names <- value
  x
})


# setMethod("names","data.set",function(x)names(x@frame))
# setReplaceMethod("names","data.set",
#   function(x,value){
#     names(x@frame) <- value
#     x
#   })

setMethod("dimnames","data.set",function(x)
  list(x@row_names,x@names))

setReplaceMethod("dimnames","data.set",function(x,value) {
    d <- dim(x)
    if (!is.list(value) || length(value) != 2L)
        stop("invalid 'dimnames' given for data set")
    value[[1L]] <- as.character(value[[1L]])
    value[[2L]] <- as.character(value[[2L]])
    if (d[[1L]] != length(value[[1L]]) || d[[2L]] != length(value[[2L]]))
        stop("invalid 'dimnames' given for data set")
    row.names(x) <- value[[1L]]
    names(x) <- value[[2L]]
    x
})

# "[.data.set" <- function(...){
#   res <- `[.data.frame`(...)
#   if(is.list(res))
#     asS4(res)
#   else res
# }

# setMethod("[",signature(x="data.set",i="ANY",j="ANY",drop="ANY"),
#   function(x,i,j,...,drop=TRUE){
#     new("data.set",
#       frame=frame[i=i,j=j,drop=drop],
#       document=x@document
#       )
# })


setMethod("[",signature(x="data.set",i="atomic",j="atomic",drop="ANY"),
  function(x,i,j,...,drop=FALSE){
    frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
    frame <- frame[i,j,drop=drop]
    if(is.data.frame(frame))
      new("data.set",
        unclass(frame),
        document=x@document
        )
    else
      frame
})

setMethod("[",signature(x="data.set",i="atomic",j="missing",drop="ANY"),
  function(x,i,j,...,drop=FALSE){
#     cat("\ndata.set,atomic,missing\n")
    Narg <- nargs()-!missing(drop)
    frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
    if(Narg > 2){
      frame <- frame[i,,drop=drop]
      if(!is.data.frame(frame))
        frame
      else
        new("data.set",
          unclass(frame),
          document=x@document
          )
    }
    else {
      frame <- frame[i]
      if(!is.data.frame(frame))
        frame
      else
        new("data.set",
          unclass(frame),
          document=x@document
          )
    }
})

setMethod("[",signature(x="data.set",i="missing",j="atomic",drop="ANY"),
  function(x,i,j,...,drop=FALSE){
#     cat("\ndata.set,missing,atomic\n")
    frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
    frame <- frame[,j,drop=drop]
    if(is.data.frame(frame))
      new("data.set",
        unclass(frame),
        document=x@document
        )
    else
      frame
})

setMethod("[",signature(x="data.set",i="missing",j="missing",drop="ANY"),
  function(x,i,j,...,drop=FALSE){
    frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
    frame <- frame[,,drop=drop]
    if(is.data.frame(frame))
      new("data.set",
        unclass(frame),
        document=x@document
        )
    else
      frame
})


setReplaceMethod("[",signature(x="data.set",i="ANY",j="ANY",value="ANY"),
  function(x,i,j,value){
    frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
    frame[i,j] <- value
    new("data.set",
      unclass(frame),
      document=x@document
      )
})


# setMethod("[[","data.set",
#   function(x,...){
#     new("data.set",
#       frame=x@frame[[...]],
#       document=x@document
#       )
# })
#
# setReplaceMethod("[[","data.set",
#   function(x,i,j,value){
#     frame <- x@frame
#     frame[[i,j]] <- value
#     new("data.set",
#       frame=frame,
#       document=x@document
#       )
# })

# "[[.data.set" <- function(x,...){
#   x@frame[[...]]
# }

"[[<-.data.set" <- function(x,...,value){
  frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
  frame[[...]] <- value
  new("data.set",
    unclass(frame),
    document=x@document
    )
}

# setMethod("$","data.set",function(x,name)x@frame[[name,exact=TRUE]])

# as.data.frame.data.set <- function(x, row.names = NULL, optional = FALSE, ...){
#   as.data.frame.list(x,row.names=row.names,optional=optional)
# }

as.list.data.set <- function(x,...)structure(x@.Data,names=x@names)

as.data.frame.data.set <- function(x, row.names = NULL, optional = FALSE, ...){
  as.data.frame(as.list(x),
          row.names=if(length(row.names)) rownames
                    else x@row_names,
          optional=optional)
}


# setMethod("as.data.frame","data.set",
#   function(x, row.names = NULL, optional = FALSE, ...){
#   as.data.frame(as.list(x),
#           row.names=if(length(row.names)) rownames
#                     else x@row_names,
#           optional=optional)
# })

data.set <- function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
    stringsAsFactors = default.stringsAsFactors(),
    document = NULL){

  args <- list(...)
  if(!length(names(args))){

    subst <- substitute(list(...))
    names(args) <- as.character(subst[-1])
  }
  argn <- names(args)
  args <- lapply(seq_along(args),function(i){
      x <- args[[i]]
      n <- names(args)[[i]]
      if(is(x,"item.vector"))
        structure(list(x),class="data.frame",row.names=seq_len(length(x)),names=n)
      else if(is(x,"data.set"))
        structure(as.list(x),class="data.frame",row.names=x@row_names)
      else x
    })
  names(args) <- argn
  frame <- do.call(data.frame,
    c(args,
      row.names=row.names,
      check.rows=check.rows,
      check.names=check.names
    ))
  new("data.set",
    frame,
    document=as.character(document)
    )
}


setMethod("annotation","data.set",function(x){
  d <- lapply(x,annotation)
  if(length(d))
    structure(d,names=x@names,class="annotation.list")
  else NULL
})

# print.description.list <- function(x,...){
#   writeLines(paste(names(x),": ",sQuote(x),sep=""))
# }

print.data.set <- function(x,max.obs=Inf,width=Inf,...){
  nrow.x <- nrow(x)
  frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
  if(is.finite(max.obs)){
    if(nrow(x)<=max.obs)
      {
        max.obs <- Inf
        res <- frame
      }
    else
      res <- frame[seq_len(max.obs),,drop=FALSE]
  }
  else
    res <- frame
  dn <- dimnames(res)
  res <- lapply(res,format)
  attr(res,"class") <- "data.frame"
  row.names(res) <- dn[[1]]

  if(is.finite(width) && ncol(res)){
    width <- width - (if(nrow(res)) max(nchar(attr(res,"row.names"))) else 0) - 5
    names.w <- cumsum(nchar(names(res),"w")+1)
    var.w <- cumsum(nchar(sapply(res,"[",1),"w")+1)
    if(any(names.w > width) || any(var.w > width)){
      dots <- "..."
      width <- width - nchar(dots,"w") - 2
      ww <- ifelse(names.w > var.w,names.w,var.w)
      use <- seq_len(max(which(ww < width)))
      res <- res[use]
      res$... <- rep("...",nrow(res))
    }
  }

  if(is.finite(max.obs) && nrow(res)){
    print(res)
    mkdots <- function(n) paste(rep(".",n),collapse="")
    rnw <- max(nchar(attr(res,"row.names")))
    var.w <- nchar(sapply(res,"[",1),"w")
    names.w <- nchar(names(res),"w")
    ww <- ifelse(names.w > var.w,names.w,var.w)
    cat(sapply(c(rnw,ww),mkdots))
    cat("\n(",nrow(res)," of ",nrow.x," observations shown)\n",sep="")
    }
  else print(res)
}

setMethod("show","data.set",function(object){
  cat("\nData set with",nrow(object),"observations and",ncol(object),"variables\n\n")
#   d <- description(object)
#   if(length(d)){
#     print(d)
#     cat("\n")
#   }
  print.data.set(object,max.obs=getOption("show.max.obs"),width=getOption("width"))
})

setMethod("print","data.set",function(x,...)print.data.set(x,...))

# summary.data.set <- summary.data.frame

#copied and modified from base
setMethod("summary","data.set",
  function(object, maxsum = 7, digits = max(3, getOption("digits") -3), ...){
    z <- lapply(as.list(object), summary, maxsum = maxsum, digits = 12,
        ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- max(unlist(lapply(z, NROW)))
    for (i in 1:nv) {
        sms <- z[[i]]
        if (is.matrix(sms)) {
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms)),
                sep = ".")
            tmp <- format(sms)
            if (nrow(sms) < nr)
                tmp <- rbind(tmp, matrix("", nr - nrow(sms),
                  ncol(sms)))
            sms <- apply(tmp, 1, function(x) paste(x, collapse = "  "))
            wid <- sapply(tmp[1, ], nchar, type = "w")
            blanks <- paste(character(max(wid)), collapse = " ")
            pad0 <- floor((wid - nchar(cn, type = "w"))/2)
            pad1 <- wid - nchar(cn, type = "w") - pad0
            cn <- paste(substring(blanks, 1, pad0), cn, substring(blanks,
                1, pad1), sep = "")
            nm[i] <- paste(cn, collapse = "  ")
            z[[i]] <- sms
        }
        else {
            lbs <- format(names(sms))
            sms <- paste(lbs, ":", format(sms, digits = digits),
                "  ", sep = "")
            lw[i] <- nchar(lbs[1], type = "w")
            length(sms) <- nr
            z[[i]] <- sms
        }
    }
    z <- unlist(z, use.names = TRUE)
    dim(z) <- c(nr, nv)
    blanks <- paste(character(max(lw) + 2), collapse = " ")
    pad <- floor(lw - nchar(nm, type = "w")/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(z) <- list(rep.int("", nr), nm)
    attr(z, "class") <- c("table")
    z
})

is.data.set <- function(x) is(x,"data.set")

str.data.set <- function (object, ...)
{
    cat("Data set ","with ", nrow(object), " obs. of ", (p <- ncol(object)),
        " variable", if (p != 1)
            "s", if (p > 0)
            ":", "\n", sep = "")
    object <- structure(as.list(object),class="data.frame")
    if (length(l <- list(...)) && any("give.length" == names(l)))
      invisible(NextMethod("str", ...))
    else invisible(NextMethod("str", give.length = FALSE, ...))
}

# subset.data.set <- subset.data.frame
setMethod("subset","data.set",
  function(x,...){
    frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
    new("data.set",
      subset(frame,...),
      document=x@document
      )
})



setMethod("within","data.set",function (data, expr, ...)
{
    parent <- parent.frame()
    frame <- structure(data@.Data,row.names=data@row_names,names=data@names,class="data.frame")
    e <- evalq(environment(), frame, parent)
    nr <- nrow(frame)
    rn <- row.names(frame)
    ret <- eval(substitute(expr), e)
    l <- rev(as.list(e))

    wrong.length <- sapply(l,length) != nr
    if(any(wrong.length)){
      warning("Variables ",paste(sQuote(names(l)[wrong.length]),collapse=","),
                " have wrong length, removing them.")
      l[wrong.length] <- NULL
    }
    coercable <- sapply(l,is.atomic) | sapply(l,is.factor)
    items <- sapply(l,is,"item")
    if(any(!items & coercable))
      l[!items & coercable] <- lapply(l[!items & coercable],as.item)
    if(any(!items & !coercable)){
      warning("Cannot change variables ",paste(sQuote(names(l)[!items & !coercable]),collapse=","),
            " into items, removing them.")
      l[!items & !coercable] <- NULL
    }
    frame[names(l)] <- l
    use <- names(frame) %in% names(l)
    frame <- frame[use]
    row.names(frame) <- rn
    new("data.set",
      frame,
      document=data@document)
})

cbind.data.set <- function (..., deparse.level = 1)
  data.set(..., check.names = FALSE)

setMethod("description","data.set",function(x){
  res <- lapply(x,description)
  res <- sapply(res,function(des){
          if(length(des)) sQuote(des)
          else " (none) "
          })
  structure(res,class="descriptions")
})
print.descriptions <- function(x,quote=FALSE,...){
  ans <- c(
      "",
      paste("",format(names(x),justify="left"),format(x,justify="left")),
      ""
      )
  writeLines(ans)
}

setMethod("unique","data.set",function(x, incomparables = FALSE, ...){
  frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
  new("data.set",
      unique(frame,incomparables=incomparables,...),
      document=x@document
      )
})


fapply.data.set <- function(formula,data,...)
  fapply.default(formula,data=as.data.frame(data,optional=TRUE),...)