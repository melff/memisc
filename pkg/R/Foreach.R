prep.vars <- function(values,parent,sorted){

    colon <- function(x) {
        if(is.numeric(x[[2]]) && is.numeric(x[[3]]))
            return(seq.int(from=x[[2]],to=x[[3]]))
        from.name <- as.character(x[[2]])
        to.name <- as.character(x[[3]])
        nms <- ls(parent,sorted=sorted)
        from <- match(from.name,nms,nomatch=0L)
        to <- match(to.name,nms,nomatch=0L)
        if(any(c(from,to)==0))stop("Unknown variable referenced")
        values <- nms[from:to]
        lapply(values,as.symbol)
    }
    tosymbols <- function(x){
        x <- as.list(x)
        isc <- sapply(x,is.call)
        if(!any(isc))return(x)
        else {
            xisc <- x[isc]
            x[isc] <- lapply(xisc,expcall)   
        }
        x
    }
    expcall <- function(x){
        if(as.character(x[[1]]) %in% c("c","list")) tosymbols(x[-1])
        else if(as.character(x[[1]])=="rx") {
            rxcall <- x
            rxcall[[1]] <- as.name("rx2symbols")
            eval(rxcall)
        }
        else if(as.character(x[[1]])==":"){
            colon(x)
        }
        else eval(x,parent)
    }
    firstchar <- function(x)substr(x,1,1)
    lastchar <- function(x){
        n <- nchar(x)
        substr(x,n,n)
    }
    rx2symbols <- function(pattern){
        nms <- ls(parent)
        pattern <- pattern[1]
        if(firstchar(pattern)!="^") pattern <- paste0("^",pattern)
        if(lastchar(pattern)!="$") pattern <- paste0(pattern,"$")
        nms <- grep(pattern=pattern,x=nms,value=TRUE)
        as.symbols(nms)
    }

    values <- if(is.call(values)) {
                  if(as.character(values[[1]]) %in% c("c","list")) tosymbols(values[-1])
                  else if(as.character(values[[1]])=="rx") {
                      rxcall <- values
                      rxcall[[1]] <- as.name("rx2symbols")
                      eval(rxcall)
                  }
                  else if(as.character(values[[1]])==":"){
                      colon(values)
                  }
                  else eval(values,parent)
              }
              else
                  eval(values)

    values <- unlist(values)
    valchars <- lapply(values,as.character)
    if(!all(nzchar(valchars))) stop("empty element in substitution list")
    values
}


foreach <- function(...,.sorted,.outer=FALSE){
  args <- match.call(expand.dots=FALSE)$...
  tags <- names(args)
  parent <- parent.frame()
  vars <- args[nzchar(tags)]
  expr <- args[!nzchar(tags)]
  if(length(expr)) expr <- expr[[1]]
  else return()
  tags <- tags[nzchar(tags)]
  if(!length(expr) || !length(tags))return(invisible(NULL))
  if(missing(.sorted)){
      if(identical(parent,.GlobalEnv))
          .sorted <- TRUE
      else
          .sorted <- FALSE
  }
  vars <- lapply(vars,prep.vars,parent=parent,sorted=.sorted)
  if(length(vars)==1) {
    vars <- as.matrix(vars[[1]])
    colnames(vars) <- tags
  } else {
      l <- sapply(vars,length)
      if(.outer) {
          i <- lapply(l,seq.int)
          i <- do.call('expand.grid',i)
          vars <- mapply('[',vars,i)
      } else {
          if(length(unique(l)) > 1) stop("variables have unequal length")
          vars <- do.call('cbind',vars)
      }
  }

  for(i in seq_len(nrow(vars))){
    subst <- vars[i,]
    if(!is.list(subst)) 
        subst <- structure(as.list(subst),
                           names=colnames(vars))
    res <- do.call("substitute",list(expr,subst))
    eval(res,parent.frame())
    }
}

xapply <- function(...,.sorted,simplify=TRUE,USE.NAMES=TRUE,.outer=FALSE){
  args <- match.call(expand.dots=FALSE)$...
  tags <- names(args)
  parent <- parent.frame()
  vars <- args[nzchar(tags)]
  expr <- args[!nzchar(tags)]
  if(length(expr)) expr <- expr[[1]]
  else return()
  tags <- tags[nzchar(tags)]
  if(!length(expr) || !length(tags))return(invisible(NULL))
  parent <- parent.frame()
  e <- evalq(environment(), list(), parent)
  if(missing(.sorted)){
      if(identical(parent,.GlobalEnv))
          .sorted <- TRUE
      else
          .sorted <- FALSE
  }
  vars <- lapply(vars,prep.vars,parent=parent,sorted=.sorted)
  vars. <- lapply(vars,as.character)
  if(length(vars)==1) {
    vars <- as.matrix(vars[[1]])
    colnames(vars) <- tags
  } else {
      l <- sapply(vars,length)
      if(.outer) {
          i <- lapply(l,seq.int)
          i <- do.call('expand.grid',i)
          vars <- mapply('[',vars,i)
      } else {
          if(length(unique(l)) > 1) stop("variables have unequal length")
          vars <- do.call('cbind',vars)
      }
  }

  res <- Sapply(seq_len(nrow(vars)),function(i){
    subst <- vars[i,]
    if(!is.list(subst)) 
        subst <- structure(as.list(subst),
                           names=colnames(vars))
    res <- do.call("substitute",list(expr,subst))
    eval(res,e)
    },simplify=simplify)
  if(USE.NAMES){
      if(isTRUE(USE.NAMES)) USE.NAMES <- 1
      else USE.NAMES <- as.integer(USE.NAMES)
      nms <- as.character(vars[,USE.NAMES])
  }
  if(simplify && is.array(res)){
      ndim <- length(dim(res))
      dmn <- dimnames(res)
      if(.outer){
          dim(res) <- c(dim(res)[-ndim],l)
          ll <- length(l)
          if(USE.NAMES){
              dimnames(res) <- c(dmn[-ndim],vars.)
          }
          else {
              dimnames(res) <- c(dmn[-ndim],rep(list(NULL),ll))
          }
      }
      else if(USE.NAMES) 
          dimnames(res)[[ndim]] <- nms
  } else if(USE.NAMES){
      names(res) <- nms
  }
  res
}

syms <- function(...,paste=FALSE,sep=""){
  sep <- as.character(sep)
  args <- match.call(expand.dots=FALSE)$...
  parent <- parent.frame()
  args <- if(!length(args)) {
        tmp <- ls(parent.frame())
        i <- grep("^_",tmp)
        if(length(i)) tmp[-i] else tmp
      }
      else 
        lapply(args,function(arg){
          if(length(arg)>1) {
            if(is.call(arg)){
              if(as.character(arg[[1]]) %in% c("c","list")) as.character(arg[-1])
              else as.character(eval(arg,parent))
            }
            else as.character(arg)
          }
          else as.character(arg)
          })
  if(paste){
    res <- do.call("paste",c(args,list(sep=sep)))
    lapply(res,as.symbol)
  }
  else 
    lapply(args,as.symbol)
}

as.symbols <- function(x) lapply(x,as.symbol)


Pairs <- function(x,y=x){
  res <- outer(x,y,function(x,y)mapply(c,x,y,SIMPLIFY=FALSE))
  t(matrix(unlist(res[lower.tri(res)],recursive=FALSE),nrow=2)[2:1,])
}






