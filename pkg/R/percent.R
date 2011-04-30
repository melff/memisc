percent <- function(x,...) UseMethod("percent")

percent.default <- function(x,weights=NULL,total=!(se || ci),
                      se=FALSE,ci=FALSE,ci.level=.95,
                      total.name="N",perc.label="Percentage",...){
  subst <- substitute(x)
  x.label <- paste(deparse(subst))
  tab <- Table(x,weights=weights,total=FALSE,...)
  tabsum <- sum(tab)
  perc <- drop(100*tab/tabsum)
  #names(perc) <- rownames(tab)

  if(total.name %in% names(perc)) total.name <- paste("_",total.name,"_",sep="")

  if(!se && !ci){
    if(total)
      perc <- c(perc,structure(tabsum,names=total.name))
    perc <- as.matrix(perc)
    colnames(perc) <- x.label
    drop(perc)
  }
  else {
      perc <- t(as.matrix(perc))
      rownames(perc) <- perc.label
      prop <- tab/tabsum
      var.prop <- prop*(1-prop)/tabsum
      se.perc <- 100*sqrt(var.prop)
    if(se){
      perc <- rbind(perc,se=se.perc)
    }
    if(ci){
      alpha <- (1-ci.level)/2
      lower <- upper <- numeric(length(tab))
      isnull <- tab == 0
      isfull <- tab == tabsum
      lower[!isnull] <- qbeta(alpha,tab[!isnull],tabsum-tab[!isnull]+1)
      lower[isnull] <- 0
      upper[!isfull] <- qbeta(1-alpha,tab[!isfull]+1,tabsum-tab[!isfull])
      upper[isfull] <- 1
      #lower <- qnorm(alpha,mean=prop,sd=sqrt(var.prop))
      #upper <- qnorm(1-alpha,mean=prop,sd=sqrt(var.prop))
      perc <- rbind(perc,lower=100*lower,upper=100*upper)
    }
    if(total){
      total <- numeric(nrow(perc))
      total[] <- tabsum
      total <- t(as.matrix(total))
      rownames(total) <- total.name
      perc <- cbind(perc,t(total))
    }
    names(dimnames(perc))[2] <- x.label
    names(dimnames(perc))[is.na(names(dimnames(perc)))] <- ""
    drop(perc)
  }
}

percent.logical <- function(x,weights=NULL,total=!(se || ci),
                      se=FALSE,ci=FALSE,ci.level=.95,
                      total.name="N",perc.label="Percentage",...){
  subst <- substitute(x)
  x.label <- paste(deparse(subst))
  tab <- drop(Table(x,weights=weights,total=FALSE,...))
  N <- sum(tab)
  n <- tab["TRUE"]
  p <- n/N
  perc <- structure(c(100*p),names=perc.label)
  if(se){
    var <- p*(1-p)/N
    perc <- c(perc,se=100*sqrt(var))
  }
  if(ci){
    if(is.finite(p)){
      alpha <- (1-ci.level)/2
      lower <- if(p) qbeta(alpha,n,N-n+1) else 0
      upper <- if(p<1) qbeta(1-alpha,n+1,N-n) else 1
      perc <- c(perc,lower=100*lower,upper=100*upper)
    }
    else
      perc <- c(perc,lower=NA,upper=NA)
  }
  if(total){
    perc <- c(perc,structure(N,names=total.name))
  }
  perc <- as.matrix(perc)
  colnames(perc) <- x.label
  drop(perc)
}

