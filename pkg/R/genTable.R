
unarray <- function(x,fill=FALSE){
  x <- drop(x)
  if(!length(dim(x))) return(x)
  if(length(dim(x))==1 || !length(dimnames(x))) return(c(x))
  dims <- dim(x)
  dimnames <- dimnames(x)
  for(i in seq_along(dimnames)){
    if(!length(dimnames[[i]]))
      if(fill)
        dimnames[[i]] <- format(seq_len(dims[i]))
      else
        dimnames[[i]] <- character(dims[i])
    }
  names <- c(reduce(dimnames,outer,dotpaste))
  structure(c(x),names=names)
}

dotpaste <- function(x,y)ifelse(nchar(x)&nchar(y),paste(x,y,sep="."),paste(x,y,sep=""))



has.response <- function(formula,data=NULL){
  #if(length(dim(data))) data <- data[1,]
  as.logical(attr(terms(formula,data=data),"response"))
}


mk.ixes <- function(dims){
 ijk <- as.matrix(seq(dims[1]))
 if(length(dims)>1){
  for(i in 2:length(dims)){
    tmp.nrow <- nrow(ijk)
    ijk <- ijk[rep(seq(nrow(ijk)),dims[i]),]
    ijk <- cbind(ijk,
              rep(
                  seq(dims[i]),
                  rep(tmp.nrow,dims[i])
                  )
                )
  }
 }
 ijk
}


genTable <- function (formula,
                      data=parent.frame(),
                      subset=NULL,
                      names=NULL,
                      addFreq=TRUE,
                      ...){

    m <- match.call(expand.dots = FALSE)
    dots <- m$...
    if(length(formula) < 3){
        m[[1]] <- as.name("xtabs")
        m[c("names","addFreq")] <- NULL
        m$... <- NULL
        return(eval(substitute(m),enclos=parent.frame()))
    }

    parent <- parent.frame()

    if(is.table(data)) data <- as.data.frame(data)
    else if(is.environment(data)){
        tmp <- try(as.data.frame(data),silent=TRUE)
        if(inherits(tmp,"try-error")) {
            tmp <- try(as.data.frame(as.list(data)),silent=TRUE)
            if(inherits(tmp,"try-error")) {
                mf <- m
                mf[[1]] <- as.name("model.frame.default")
                mf$x <- NULL
                mf$formula <- as.formula(paste("~",paste(all.vars(formula),collapse="+")))
                mf$data <- data
                mf$... <- mf$names <- mf$addFreq <- mf$as.vars <- NULL
                data <- eval(mf,parent)
            }
        }
        else
            data <- tmp
    }

    if(!missing(subset)){
        subset <- eval(substitute(subset),data,parent)
        if(is.logical(subset))
            subset <- subset & !is.na(subset)
        else stop("'subset' arg must be logical")
        data <- data[subset,,drop=FALSE]
    }
    

    by <- formula[-2]
    expr <- formula[[2]]

    if(length(expr)==1){
        expr.c <- as.character(expr)
        if(is.factor(data[[expr.c]]))
            expr <- as.call(c(as.symbol("table"),expr))
        else
            expr <- as.call(c(as.symbol("sum"),expr))
    }
    if(addFreq){
        if("Freq" %in% names(data) &&length(expr) > 1 && 
           as.character(expr[[1]]) %in% c("table","Table","percent","nvalid") &&
           !("weights" %in% names(expr))
           ){
            if(as.character(expr[[1]])=="table")
                expr[[1]] <- as.symbol("Table")
            expr[[3]] <- as.symbol("Freq")
        }
    }

    
    if(deparse(formula[[3]])=="."){
        vars <- setdiff(names(data),all.vars(expr))
        by <- as.formula(paste("~",paste(vars,collapse="+")))
    }
    if(length(dots)) expr <- as.call(c(as.list(expr),dots))
    
    gdata <- Groups(data=data,by=by)
    
    wcall <- call("with",data=gdata,expr=expr)
    res <- eval(wcall,enclos=parent.frame())
    spec <- attr(res,"spec")
    if(length(dim(res)) > length(spec) && !missing(names))
        dimnames(res)[[1]] <- names
    res
}

Aggregate <- function (formula,
                       data=parent.frame(),
                       subset=NULL,
                       names=NULL,
                       addFreq=TRUE,
                       drop=TRUE,
                       as.vars=1,
                       ...){

    m <- match.call()
    # if(length(formula) < 3 || length(formula[[2]]) < 2){
    #     m[[1]] <- as.name("xtabs")
    #     m[c("names","addFreq")] <- NULL
    #     tab <- eval(substitute(m),enclos=parent.frame())
    #     return(as.data.frame(tab))
    # }

    m[[1]] <- as.name("genTable")
    res <- eval(m,enclos=parent.frame())

    empty <- attr(res,"empty")
    spec <- attr(res,"spec")
    if(length(dim(res)) > length(spec)){
        res <- to.data.frame(res,as.vars=as.vars)
    }
    else {
        res <- as.data.frame(res)
        if(!missing(names))
            names(res)[3L] <- names
        else
            names(res)[3L] <- deparse(formula[[2]])
    }
    if(drop && any(empty)){
        nr <- nrow(res)
        ne <- length(empty)
        emtpy <- rep(as.vector(empty),each=nr/ne)
        res <- res[!empty,,drop=FALSE]
    }
    res
}
