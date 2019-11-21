view <- function(x,
                 title=deparse(substitute(x)),
                 vfunc=getOption("vfunc","View"),
                 ...)
{

    # cls <- class(x)
    # title <- paste0(cls,": ",title)

    prepd <- viewPrep(x,title,...)
    title <- attr(prepd,"title")
    
    View.call <- call(vfunc,x=prepd,title=title)
    eval(View.call,globalenv())
}

viewPrep <- function(x,title,...)UseMethod("viewPrep")
viewPrep.default <- function(x,title,...){
    return(structure(x,title=title))
}

viewPrep.data.set <- function(x,title,...){
    # title <- paste("Data set:",title)
    Data <- lapply(x@.Data,format,justify="left")
    frame <- structure(Data,
                       row.names=x@row_names,
                       names=x@names,
                       class="data.frame")
    for(n in names(frame)){
        d <- description(x[[n]])
        if(length(d))
            attr(frame[[n]],"label") <- d
    }
    return(structure(frame,title=title))
}

viewPrep.data.frame <- function(x,title,...){
    structure(x,title=title)
}

viewPrep.descriptions <- function(x,title,...){
  # title <- paste("Descriptions:",title)
  viewPrep.data.frame(as.data.frame(x),
                      title=title,
                      ...)
}

parse_string <- function(x)parse(file=NULL,text=x)
format_lab_view <- function(x){
  fmt <- paste(unname(x),sQuote(names(x)))
  fmt <- paste(fmt,collapse=", ")
  return(fmt)
}
in_range <- function(x,rng){
    x >= rng[1] & x <= rng[2]
}

view1cp <- function(x){
    n <- x["name"]
    d <- x["description"]
    l <- x["labels"]
    if(nzchar(l))
        l <- eval(parse(text=l))
    else
        l <- NULL
    ms <- x["measurement"]
    vf <- x["value.filter"]
    if(nzchar(vf)){
        vf <- paste0("list(",vf,")")
        vf <- eval(parse(text=vf))
        cl <- names(vf)
        vf <- vf[[1]]
        if(cl=="missing")
            vf <- new("missing.values",
                      filter=vf$values,
                      range=vf$range
                      )
        else if(cl=="valid"){
            if(length(vf$values))
                vf <- new("valid.values",filter=vf$values)
            else if(length(vf$range))
                vf <- new("valid.range",filter=vf$range)
        }
    } else vf <- NULL
    nl <- length(l)
    tab <- matrix("",ncol=4,nrow=max(1,nl))
    colnames(tab) <- c("variable","description","value","label")
    tab[1,1] <- n
    tab[1,2] <- d
    omtab <- NULL
    if(nl>0){
      if(length(vf))
          ism <- is.missing2(l,vf)
      else
          ism <- FALSE
      tab[1:nl,3] <- paste(unname(l),ifelse(ism,"M"," "))
      tab[1:nl,4] <- names(l)
      if(length(vf) && inherits(vf,"missing.values")){
          if(length(vf@filter) && any(vf@filter %nin% l)){
              omiss <- setdiff(vf@filter,l)
              omiss <- paste(omiss,"M")
              omtab <- rbind(omtab,cbind("","",omiss,"(unlabelled)"))
          }
      #     if(length(vf@range) && any(!in_range(l,vf@range))){
      #         omiss <- paste(vf@range[1],"--",vf@range[2],"M")
      #         omtab <- rbind(omtab,cbind("","",omiss,"(unlabelled)"))
      #     }
      } 
    } else if(length(vf)){
        if(inherits(vf,"missing.values")){
          if(length(vf@filter)){
              omiss <- setdiff(vf@filter,l)
              omiss <- paste(omiss,"M")
              omtab <- rbind(omtab,cbind("","",omiss,"(unlabelled)"))
          }
          if(length(vf@range)){
              omiss <- paste(vf@range[1],"--",vf@range[2],"M")
              omtab <- rbind(omtab,cbind("","",omiss,"(unlabelled)"))
          }
        } else if(inherits(vf,"valid.values")){
            nvd <- length(vf@filter)
            if(nvd > 1)
                tab <- rbind(tab,matrix("",ncol=ncol(tab),nrow=nvd-1))
            tab[1:nvd,3] <- paste(vf@filter," ")
            tab[1:nvd,4] <- "(unlabelled)"
        } else if(inherits(vf,"valid.range")){
            tab[1,3] <- paste(vf@filter[1],"--",vf@filter[2]," ")
            tab[1:nvd,4] <- "(unlabelled)"
        }
    }
    
    if(length(omtab)){
        tab <- rbind(tab,omtab)
    }
    return(tab)
}

viewPrep.codeplan <- function(x,title,compact=FALSE,...){
    # title <- paste("Code plan:",title)
    if(compact){
      labels <- x$labels
      nll <- nzchar(labels)
      labels <- as.list(labels)
      labels[nll] <- lapply(labels[nll],parse_string)
      labels[nll] <- lapply(labels[nll],eval)
      labels[nll] <- lapply(labels[nll],format_lab_view)
      labels <- unlist(labels)
      frame <- data.frame(variable=x$name,
                          description=x$description,
                          labels=labels,
                          value.filter=x$value.filter)
      viewPrep.data.frame(frame,
                          title=title,
                          ...)
    }
    else {
        if(nrow(x)>1){
            tabs <- apply(x,1,view1cp)
            tab <- do.call(rbind,tabs)
        }
        else
            tab <- view1cp(unlist(x))
            
      tab[,3] <- format(trimws(tab[,3]),justify="right",width=5)
      viewPrep.default(tab,
                       title=title,
                       ...)
    }      
}

viewPrep.importer <- function(x,title,compact=TRUE,...){
    cp <- codeplan(x)
    viewPrep.codeplan(x=cp,title=title,compact=compact,...)
}
