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
  else if(any(length(object@row_names) != sapply(object,length))){
    wrong.els <- object[!isItemVector]
    wrong.names <- object@names[!isItemVector]
    wront.lengths <- sapply(object,length)
    paste(
    if(length(which(wrong.lengths)) > 1) "elements have" else "element has",
    "wrong length: ",
      paste(
        paste("class(",wrong.names,") = ",wrong.classes,sep=""),
        collapse=", "
      ),
    "where",
    length(object@row_names),
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

  args <- list(...)
  if(is.list(args[[1]])) args <- unclass(args[[1]])
  nr <- max(sapply(args,length))
  args <- lapply(args,setLength,n=nr)
  args <- lapply(args,as.item)

  .Object@.Data <- unname(args)
  .Object@names <- as.character(names(args))

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

  if(validObject(.Object)) .Object
})

setAs("data.set","named.list",function(from,to){
  new(to,structure(from@.Data,names=from@names))
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



"[[<-.data.set" <- function(x,...,value){
  frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")
  frame[[...]] <- value
  new("data.set",
    unclass(frame),
    document=x@document
    )
}

as.list.data.set <- function(x,...)structure(x@.Data,names=x@names)

as.data.frame.data.set <- function(x, row.names = NULL, optional = FALSE, ...){
  as.data.frame(as.list(x),
          row.names=if(length(row.names)) rownames
                    else x@row_names,
          optional=optional)
}



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
      check.names=check.names,
      stringsAsFactors=stringsAsFactors
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

  varn <- names(res)
  rown <- rownames(res)

  res <- lapply(res,format)
  res <- mapply(c,varn,res)
  res <- apply(res,2,format,justify="right")
  res <- apply(cbind(c("",rown),res),2,format,justify="right")
  
  if(is.finite(width) && ncol(res)){

    ww <- cumsum(nchar(res[1,])+1)-1
    if(any(ww > width)){

      keep <- which(ww < width - 3)
      res <- cbind(res[,keep],"...")
    }
  }

  if(is.finite(max.obs) && nrow(res)){

    mkdots <- function(n) paste(rep(".",n),collapse="")
    ww <- nchar(res[1,])
    res <- rbind(res,sapply(ww,mkdots))
    res <- apply(res,1,paste,collapse=" ")
    res <- c(res,paste("(",length(res)-2," of ",nrow.x," observations shown)",sep=""))
    }
  else
    res <- apply(res,1,paste,collapse=" ")

  writeLines(res)
}

setMethod("show","data.set",function(object){
  cat("\nData set with",nrow(object),"observations and",ncol(object),"variables\n\n")
  print.data.set(object,max.obs=getOption("show.max.obs"),width=getOption("width"))
})

setMethod("print","data.set",function(x,...)print.data.set(x,...))

# summary.data.set <- summary.data.frame

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
    
    
    assign(".nobs.",length(data@row_names),parent)
    assign(".nvars.",length(data@names),parent)
    assign(".id.",1:get(".nobs.",parent),parent)
    
    
    frame <- structure(data@.Data,row.names=data@row_names,names=data@names,class="data.frame")
    e <- evalq(environment(), frame, parent)
    nr <- nrow(frame)
    rn <- row.names(frame)
    ret <- eval(substitute(expr), e)
    l <- rev(as.list(e))

    length1 <- sapply(l,length) == 1
    if(any(length1)){
      ii <- which(length1)
      for(i in ii){
        l[[i]] <- rep(l[[i]],nr)
      }
    }
    
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
  structure(res,class="descriptions")
})
print.descriptions <- function(x,quote=FALSE,...){
  Write.descriptions(x,file=stdout())
}

Write.descriptions <- function(x,file=stdout(),...){
  x <- sapply(x,function(des){
          if(length(des)) sQuote(des)
          else " (none) "
          })
  out <- c(
    "",
    paste("",format(names(x),justify="left"),format(x,justify="left")),
    ""
  )
  writeLines(out,con=file)
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
  
setMethod("as.data.set","list",function(x,row.names=NULL,...){
  class(x) <- "data.frame"
  if(length(row.names)){
    if(length(row.names)!=nrow(x)) stop("row.names argument has wrong length")
    attr(x,"row.names") <- row.names
  }
  else
    attr(x,"row.names") <- seq_len(nrow(x))
  new("data.set",x)
})



setMethod("merge",signature(x="data.set","data.set"),function(x,y,...){
  x <- new("data.frame",as.list(x),row.names=x@row_names)
  y <- new("data.frame",as.list(y),row.names=y@row_names)
  z <- merge(x,y,...)
  new("data.set",z)
})

setMethod("merge",signature(x="data.set","data.frame"),function(x,y,...){
  x <- new("data.frame",as.list(x),row.names=x@row_names)
  z <- merge(x,y,...)
  data.set(z)
})

setMethod("merge",signature(x="data.frame","data.set"),function(x,y,...){
  y <- new("data.frame",as.list(y),row.names=y@row_names)
  z <- merge(x,y,...)
  data.set(z)
})

setMethod("rbind2",signature(x="data.set",y="data.set"),function(x,y){
  x <- asS4(new("data.frame",as.list(x),row.names=x@row_names),FALSE)
  y <- asS4(new("data.frame",as.list(y),row.names=y@row_names),FALSE)
  z <- rbind(x,y)
  data.set(z)
})

setMethod("rbind2",signature(x="data.set",y="data.frame"),function(x,y){
  x <- asS4(new("data.frame",as.list(x),row.names=x@row_names),FALSE)
  z <- cbind(x,y)
  data.set(z)
})

setMethod("cbind2",signature(x="data.set",y="data.set"),function(x,y){
  x <- asS4(new("data.frame",as.list(x),row.names=x@row_names),FALSE)
  y <- asS4(new("data.frame",as.list(y),row.names=y@row_names),FALSE)
  z <- cbind(x,y)
  data.set(z)
})

setMethod("cbind2",signature(x="data.frame",y="data.set"),function(x,y){
  y <- asS4(new("data.frame",as.list(y),row.names=y@row_names),FALSE)
  z <- cbind(x,y)
  data.set(z)
})

setMethod("cbind2",signature(x="data.set",y="data.frame"),function(x,y){
  x <- asS4(new("data.frame",as.list(x),row.names=x@row_names),FALSE)
  z <- cbind(x,y)
  data.set(z)
})

rbind.data.set <- function(...,deparse.level=1){
  args <- list(...)
  to.data.frame <- function(x){
    if(inherits(x,"data.set"))
      structure(
        x@.Data,
        names=x@names,
        row.names=x@row_names,
        class="data.frame"
      )
    else as.data.frame(x)
  }
  args <- lapply(args,to.data.frame)
  res <- do.call("rbind",c(args,list(deparse.level=deparse.level)))
  new("data.set",res,row.names=row.names(res))
}

dsView <- function(x){
  
  title <- paste("Data set:", deparse(substitute(x))[1])

  Data <- lapply(x@.Data,format,justify="left")
  document <- x@document
  row.names <- x@row_names
  .names <- x@names
  frame <- structure(Data,row.names=row.names,names=x@names,
                          class="data.frame")
  #View.call <- call("View",x=frame,title=title)
  #eval(View.call,globalenv())
  View(x=frame,title=title)
}



collect.data.set <- function(...,
  names=NULL,inclusive=TRUE,fussy=FALSE,warn=TRUE,
  sourcename=".origin"){
  args <- list(...)
  subst <- substitute(list(...))
  if(length(names)) {
    if(length(names)!=length(args)) stop("names argument has wrong length")
  }
  else {
    if(length(names(args))) names <- names(args)
    else {
      names <- sapply(lapply(subst[-1],deparse),paste,collapse=" ")
    }
  }
  all.vars <- lapply(args,names)
  common.vars <- reduce(all.vars,intersect)
  all.vars <- reduce(all.vars,union)
  other.vars <- setdiff(all.vars,common.vars)
  source <- rep(seq_along(args),sapply(args,nrow))
  nrow.items <- sapply(args,nrow)
  nrow.total <- sum(nrow.items)
  ix <- split(seq_len(nrow.total),source)
  res <- lapply(common.vars,function(var){
                vecs <- lapply(args,function(x)x[[var]])
                collOne(vecs,source=source,nrow.items=nrow.items,varname=var,fussy=fussy)
                })
  names(res) <- common.vars
  if(inclusive){
    res1 <- lapply(other.vars,function(var){
                  vecs <- lapply(args,function(x)x[[var]])
                  collOne(vecs,source=source,nrow.items=nrow.items,varname=var,fussy=fussy)
                  })
    names(res1) <- other.vars
    res <- c(res,res1)
  }
  res[[sourcename]] <- factor(source,labels=names)
  as.data.set(res)
}


## Copied and modified from base package
## Original copyright (C) 1995-2013 The R Core Team

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
