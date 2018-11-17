any_dup <- function(x) length(x) && any(duplicated(x@.Data))

which_dup_lab <- function(x){
    ii <- duplicated(x@.Data)
    dup_lab <- unique(x@.Data[ii])
    structure(lapply(dup_lab,get_labs,labels=x@.Data,value=x@values),
              names=dup_lab)
}

get_labs <- function(which,labels,values){
    i <- labels == which
    values[i]
}
 
duplicated_labels <- function(x) UseMethod("duplicated_labels")

duplicated_labels.item <- function(x){
    l <- labels(x)
    ii <- duplicated(l@.Data)
    if(any(ii)){
        dup_lab <- unique(l@.Data[ii])
        structure(lapply(dup_lab,
                         get_labs,
                         labels=l@.Data,
                         value=l@values),
                  names=dup_lab,
                  class="dupLabelsReport1")
    }
    else NULL
}

print.dupLabelsReport1 <- function(x,...){
    n <- paste0(names(x),":")
    l <- sapply(x,paste,collapse=", ")
    r <- cbind(format(n),format(l))
    r <- apply(r,1,paste,collapse=" ")
    writeLines(r)
}

duplicated_labels.item.list <- function(x){
    ll <- lapply(as.list(x),labels)
    ii <- sapply(ll,any_dup)
    ll <- ll[ii]
    d <- description(x)
    d <- d[ii]
    if(length(ll))
        structure(lapply(ll,which_dup_lab),
                  description=d,
                  class="dupLabelsReport")
    else NULL
}

print.dupLabelsReport <- function(x,...){
    width <- getOption("width",80)
    toprule <- paste(rep("=",width),collapse="")
    midrule <- paste(rep("-",width),collapse="")
    n <- names(x)
    d <- attr(x,"description")
    for(i in seq_along(x)){
        cat("\n",toprule,sep="")
        cat("\n ",n[i],": ",sQuote(d[i]),sep="")
        cat("\n",midrule,"\n",sep="")
        x.i <- x[[i]]
        l.i <- sapply(x.i,paste,collapse=", ")
        w <- width - max(nchar(l.i)) - 5
        n.i <- names(x.i)
        cutit <- nchar(n.i) > w
        n.i[cutit] <- paste0(substr(n.i[cutit],start=1,stop=w-3),"...")
        n.i <- paste0(n.i,":")
        r.i <- cbind(" ",format(n.i),format(l.i))
        r.i <- apply(r.i,1,paste,collapse=" ")
        writeLines(r.i)
    }
}

warn_if_duplicate_labels <- function(variables){
    ll <- lapply(variables,labels)
    ii <- sapply(ll,any_dup)
    if(any(ii)){
        n <- names(variables)[ii]
        nn <- paste(n,collapse=", ")
        nn <- strwrap(nn,prefix="  ")
        nn <- paste(nn,collapse="\n")
        warning(sprintf("%d variables have duplicated labels:\n%s",
                        length(n),
                        nn),
             call.=FALSE)
    }
}
