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

    if(is.environment(data)){
        data <- mget(all.vars(formula),
                     envir=data,
                     inherits=TRUE)
    }
    data <- as.data.frame(data)

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
