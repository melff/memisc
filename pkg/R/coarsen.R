coarsen <- function(x,...) UseMethod("coarsen")

coarsen.numeric <- function(x,
                n=5,
                pretty=TRUE,
                quantiles=!pretty,
                breaks=NULL,
                brackets=FALSE,
                sep=if(brackets)";"else if(quantiles) "-" else " - ",
                left="[",
                right="]",
                range=FALSE,
                labels=NULL,
                ...){
    if(quantiles && missing(pretty)) pretty <- FALSE
    if(pretty && quantiles) stop("Only one of 'pretty' or 'quantiles' may be TRUE.")
    if(!(pretty || quantiles)) stop("At least one of 'pretty' or 'quantiles' must be TRUE.")
    if(length(breaks)) { 
        pretty <- FALSE
        quantiles <- FALSE
    }
    lo <- min(x)
    hi <- max(x)

    if(pretty || quantiles){
        if(pretty){
            breaks <- pretty(x,n=n,...)
            lwr <- breaks[-length(breaks)]
            upr <- breaks[-1]
        }
        else if(quantiles){
            p <- seq(from=0,to=1,length=n+1)
            p <- p[!(p %in% c(0,1))]
            breaks <- quantile(x,probs=p,...)
            lab <- names(breaks)
            breaks <- c(lo,breaks,hi)
            lwr <- c("0%",lab)
            upr <- c(lab,"100%")
        }
    } else {
        if(range)
            breaks <- c(lo,breaks,hi)
        lwr <- breaks[-length(breaks)]
        upr <- breaks[-1]
    }
    if(missing(labels)){
        if(brackets)
            labels <- paste0(left,paste(lwr,upr,sep=sep),right)
        else 
            labels <- paste(lwr,upr,sep=sep)
    }
    f <- cut(x,breaks=breaks,include.lowest=TRUE)
    if(length(labels)!=nlevels(f))
        stop("Number of labels does not match the number of levels.")
    f <- factor(f,levels=levels(f),labels=labels)
    f
}
