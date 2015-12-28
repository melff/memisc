cbind.ftable <- function(..., deparse.level=1){
  
  args <- list(...)
  argnames <- names(args)
  rv <- lapply(args,attr,"row.vars")
  rv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!length(rv[[i]])){
        if(i==1) stop("first argument must by an ftable")
        if(is.matrix(args[[i]])){
          if(nrow(args[[i]])!=nrow(args[[1]])) stop("row vars must be identical")
          tmp <- args[[i]]
        }
        else{
          tmp <- matrix(NA,nrow=nrow(args[[1]]),ncol=1)
          tmp[] <- as.numeric(args[[i]])
          colnames(tmp) <- if(length(argnames)&&length(argnames[[i]]))argnames[[i]] else ""
          names(dimnames(tmp))[2] <- ""
        }
        attr(tmp,"row.vars") <- rv[[1]]
        attr(tmp,"col.vars") <- dimnames(tmp)[2]
        dimnames(tmp) <- NULL
        class(tmp) <- "ftable"
        args[[i]] <- tmp
      }
      else if(!identical(rv[[1]],rv[[i]])) {
        rv.ok <- FALSE
        break
      }
    }
  }

  if(!(rv.ok)) stop("row vars must be identical")

  row.vars <- list(attr(args[[1]],"row.vars"))
  col.vars <- lapply(args,attr,"col.vars")
  names(row.vars) <- NULL
  names(col.vars) <- NULL
  args <- lapply(args,function(arg)array(arg,dim=dim(arg)))
  res <- matrix(args,nrow=1,ncol=length(args))
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

rbind.ftable <- function(..., deparse.level=1){

  args <- list(...)
  argnames <- names(args)
  cv <- lapply(args,attr,"col.vars")
  cv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!length(cv[[i]])){
        if(i==1) stop("first argument must by an ftable")
        if(is.matrix(args[[i]])){
          if(ncol(args[[i]])!=ncol(args[[1]])) stop("col vars must be identical")
          tmp <- args[[i]]
        }
        else{
          tmp <- matrix(NA,ncol=ncol(args[[1]]),nrow=1)
          tmp[] <- as.numeric(args[[i]])
          rownames(tmp) <- if(length(argnames)&&length(argnames[[i]]))argnames[[i]] else ""
          names(dimnames(tmp))[1] <- ""
#           browser()
        }
        attr(tmp,"row.vars") <- dimnames(tmp)[1]
        attr(tmp,"col.vars") <- cv[[1]]
        dimnames(tmp) <- NULL
        class(tmp) <- "ftable"
        args[[i]] <- tmp
      }
      else if(!identical(cv[[1]],cv[[i]])) {
        cv.ok <- FALSE
        break
      }
    }
  }

  if(!(cv.ok)) stop("column vars must be identical")

  col.vars <- list(attr(args[[1]],"col.vars"))
  row.vars <- lapply(args,attr,"row.vars")
  names(row.vars) <- NULL
  names(col.vars) <- NULL
  args <- lapply(args,function(arg)array(arg,dim=dim(arg)))
  res <- matrix(args,ncol=1,nrow=length(args))
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

cbind.ftable_matrix <- function(..., deparse.level=1){

  args <- list(...)
  inh.fm <- sapply(args,inherits,"ftable_matrix")
  classes.ok <- all(inh.fm)
  if(!classes.ok) stop("all arguments must be 'ftable' or 'ftable_matrix' objects")

  rv <- lapply(args,attr,"row.vars")
  rv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!identical(rv[[1]],rv[[i]])) {
        rv.ok <- FALSE
        break
      }
    }
  }

  if(!(rv.ok)) stop("row vars must be identical")

  row.vars <- rv[[1]]
  col.vars <- do.call(c,lapply(args,attr,"col.vars"))
  res <- lapply(args,function(x){
                attributes(x)[c("col.vars","row.vars","class")] <- NULL
                x
              })
  res <- do.call(cbind,res)
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

rbind.ftable_matrix <- function(..., deparse.level=1){

  args <- list(...)
  inh.fm <- sapply(args,inherits,"ftable_matrix")
  classes.ok <- all(inh.fm)
  if(!classes.ok) stop("all arguments must be 'ftable' or 'ftable_matrix' objects")

  cv <- lapply(args,attr,"col.vars")
  cv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!identical(cv[[1]],cv[[i]])) {
        cv.ok <- FALSE
        break
      }
    }
  }

  if(!(cv.ok)) stop("column vars must be identical")

  row.vars <- do.call(c,lapply(args,attr,"row.vars"))
  col.vars <- cv[[1]]
  res <- lapply(args,function(x){
                attributes(x)[c("col.vars","row.vars","class")] <- NULL
                x
              })
  res <- do.call(rbind,res)
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

fm_mkhdr <- function(cv,quote){

  nms <- names(cv)
  l <- sapply(cv,length)
  prod.l <- cumprod(l)
  m <- length(cv)
  n <- prod.l[length(prod.l)]
  
  header <- matrix("",nrow=m,ncol=n)

  repn <- c(1,prod.l[-length(prod.l)])
  for(i in 1:m){

    ll <- prod.l[i]
    ii <- seq(from=1,length=ll,by=n%/%ll)
    header[i,ii] <- if(quote) paste0("\"",cv[[i]],"\"") else cv[[i]]
  }
  
  cbind(format(nms,justify="l"),header,"")
}

fm_mkldr <- function(rv,quote){

  nms <- names(rv)
  l <- sapply(rv,length)
  prod.l <- cumprod(l)
  m <- length(rv)
  n <- prod.l[length(prod.l)]

  header <- matrix("",nrow=n,ncol=m)

  repn <- c(1,prod.l[-length(prod.l)])
  for(i in 1:m){

    ll <- prod.l[i]
    ii <- seq(from=1,length=ll,by=n%/%ll)
    header[ii,i] <- if(quote) paste0("\"",rv[[i]],"\"") else rv[[i]]
  }

  structure(rbind(nms,header,""),dimnames=NULL)
}

to.nrow <- function(x,nrow,from=c("top","bottom")){

  from <- match.arg(from)
  res <- matrix(vector(mode=mode(x),length=length(x)),nrow=nrow,ncol=ncol(x))

  if(from=="top") res[1:nrow(x),] <- x
  else res[1:nrow(x) + nrow-nrow(x),] <- x
  res
}

to.ncol <- function(x,ncol,from=c("left","right")){

  from <- match.arg(from)
  res <- matrix(vector(mode=mode(x),length=length(x)),nrow=nrow(x),ncol=ncol)
  if(from=="left") res[,1:ncol(x)] <- x
  else res[,1:ncol(x) + ncol-ncol(x)] <- x
  res
}


format.ftable_matrix <- function(x,quote=TRUE,digits=0,format="f",...){

  d <- digits
  digits <- integer(ncol(x))
  digits[] <- d

  f <- format
  format <- character(ncol(x))
  format[] <- f

  fmt <- array(list(),dim=dim(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){

      tmp <- formatC(x[[i,j]],digits=digits[j],format=format[j])
      tmp <- cbind("",tmp,"")
      tmp <- rbind("",tmp,"")
      fmt[[i,j]] <- tmp
    }
  }

  col.vars <- attr(x,"col.vars")
  row.vars <- attr(x,"row.vars")

  header <- lapply(col.vars,fm_mkhdr,quote=quote)
  max.hh <- max(sapply(header,nrow))

  header <- lapply(header,to.nrow,nrow=max.hh,from="bottom")
  header <- do.call(cbind,header)

  leader <- lapply(row.vars,fm_mkldr,quote=quote)
  max.lh <- max(sapply(leader,ncol))
  leader <- lapply(leader,to.ncol,ncol=max.lh,from="right")
  leader <- do.call(rbind,leader)

  leader <- rbind(matrix("",nrow=nrow(header),ncol=ncol(leader)),leader)
  leader <- apply(leader,2,format.default,justify="l")
  
  body <- list()
  for(i in 1:nrow(fmt))
    body[[i]] <- do.call(cbind,fmt[i,])
  body <- do.call(rbind,body)

  #body <- apply(body,2,format.default,justify="r")
  res <- rbind(header,body)
  res <- apply(res,2,format.default,justify="r")
  res <- cbind(leader,res)
  res
}

Write.ftable_matrix <- function(x,
                            file = "",
                            quote = TRUE,
                            append = FALSE,
                            digits = 0,
                            ...){
  r <- format.ftable_matrix(x,quote=quote,digits=digits)
  r <- apply(r,1,paste,collapse=" ")
  cat(r, file = file, append = append, sep = "\n")
  invisible(x)
}

print.ftable_matrix <- function(x,quote=FALSE,...)
  Write.ftable_matrix(x,file="",quote=quote,...)

