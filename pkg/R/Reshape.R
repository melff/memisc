
unlist1 <- function(x)unlist(as.list(x[-1]))

get_vnames_or_eval <- function(x,envir=NULL,enclos=parent.frame()){
    if(inherits(x,"call") && deparse(x[[1]]) == "c"){
        return(sapply(x[-1],as.character))
    }
    else {
        res <- try(eval(x,envir=envir,enclos=enclos),silent=TRUE)
        if(inherits(res,"try-error"))
            return(NULL)
        else return(res)
    }
}

get_vnames <- function(x,envir=NULL,enclos=parent.frame()){
    if(length(x) == 1) return(as.character(x))
    else if(inherits(x,"call") && deparse(x[[1]]) == "c"){
        return(sapply(x[-1],as.character))
    }
    else {
        return(NULL)
    }
}


all_in_names <- function(x,names){
    all(!nzchar(x) | x %in% names)
}

Reshape <- function(data,...,id,within_id,drop,direction){

    mycall <- match.call(expand.dots=FALSE)

    spec <- mycall$...
    m <- length(spec)

    if(m > 1 && missing(within_id))
        vspec <- spec[-m]
    else
        vspec <- spec
    if(length(vspec)==1 &&
       deparse(vspec[[1]][[1]]) == "list"){
       vspec <- as.list(vspec[[1]][-1]) 
    }
    vspec <- lapply(vspec,get_vnames_or_eval,envir=data,enclos=parent.frame())
    v.names <- names(vspec)
    varying <- vspec
    varying <- lapply(varying,function(x){
        x[!nzchar(x)] <- "__tmp__na__"
        return(x)
    })

    if(!missing(drop))
        drop <- intersect(get_vnames(mycall$drop,envir=data),
                          names(data))
    else
        drop <- NULL
    if(!missing(within_id)){
            within_id <- intersect(get_vnames(mycall$within_id,envir=data),
                                   names(data))
            timevar <- within_id
            times <- unique(data[[timevar]])
    }
    else if(m > 1){
        times <- eval(spec[[m]],envir=data,enclos=parent.frame())
        timevar <- names(spec)[m]
    }
    else {
        n <- length(varying[[1]])
        times <- 1:n
        timevar <- "times"
    }

    if(direction=="long"){
        data[["__tmp__na__"]] <- NA
        cls <- class(data)
        class(data) <- "data.frame"
        res <- reshape(data,
                       varying=varying,
                       v.names=v.names,
                       times=times,
                       timevar=timevar,
                       drop=drop,
                       direction=direction)
        res["__tmp__na__"] <- NULL
        if(is.character(times))
            res[[timevar]] <- factor(res[[timevar]],
                                      levels=times)
        idvar <- "id"
        ii <- order(res[[idvar]],res[[timevar]])
        res <- res[ii,]
        class(res) <- cls

    } else {
        rlattr <- attr(data,"reshapeLong")
        drop_id <- FALSE
        if(missing(id)){
            if(length(rlattr) && length(rlattr$idvar))
                idvar <- rlattr$idvar
            else {
                if("id" %in% names(data)) idvar <- "id"
                else stop("missing id variable, please provide one with idvar=...")
            }
            idvars <- idvar
        }
        else {
            idvars <- mycall$id
            idvars <- get_vnames(idvars,envir=data)
            if(length(idvars)>1){
                ids <- data[,idvars,drop=FALSE]
                data[["id"]] <- as.integer(do.call(interaction,ids))
                idvar <- "id"
                drop_id <- TRUE
            }
            else idvar <- idvars
        }
        if(timevar %nin% names(data)){
            stop("missing within_id variable, please provide one.")
        }
        cls <- class(data)
        class(data) <- "data.frame"
        res <- reshape(as.data.frame(data),
                       varying=varying,
                       v.names=v.names,
                       times=times,
                       timevar=timevar,
                       idvar=idvar,
                       drop=drop,
                       direction=direction)
        varying_ <- unname(unlist(varying))
        nonvar_ <- setdiff(names(res),varying_) 
        ii <- c(nonvar_,varying_)
        attr_reshape <- attr(res,"reshapeWide")
        res <- res[ii]
        if("__tmp__na__" %in% names(res))
            res[["__tmp__na__"]] <- NULL
        else
            attr(res,"reshapeWide") <- attr_reshape
        if(drop_id)
            res[[idvar]] <- NULL
        class(res) <- cls
    }
    
    res
}



    # if(direction=="wide"){
    #     reshapeAttr <- attr(res,"reshapeLong")
    #     varying <- unlist(varying)
    #     othervars <- !(names(res) %in% varying)
    #     othervars <- names(res)[othervars]
    #     varying <- varying[varying %in% names(res)]
    #     res <- res[c(othervars,varying)]
    #     rownames(res) <- NULL
    # }
