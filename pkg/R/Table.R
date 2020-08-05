setMethod("Table",signature(x="atomic"),
      function(x,weights=NULL,counts=TRUE,percentage=FALSE,...) {
        if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
          if(!length(weights)){
            tab <- drop(table(x))
          } else {
            good <- is.finite(weights) & is.finite(x)
            tmp <- rowsum(weights[good],x[good])
            tab <- drop(tmp)
          }
        if(percentage) {
          perc <- 100 * tab/sum(tab)
        }
        structure(if(counts && percentage)
            cbind(Counts=tab,Percent=perc)
          else if(percentage)
            perc
          else
            tab
            ,class="table")
      })

setMethod("Table",signature(x="factor"),
      function(x,weights=NULL,counts=TRUE,percentage=FALSE,...) {
        if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
          if(!length(weights)){
            tab <- drop(table(x))
          } else {
            good <- is.finite(weights) & is.finite(x)
            tmp <- rowsum(weights[good],x[good])
            tab <- structure(rep(0,nlevels(x[good])),names=levels(x[good]))
            tab[rownames(tmp)] <- tmp[]
          }
        if(percentage) {
          perc <- 100 * tab/sum(tab)
        }
        structure(if(counts && percentage)
            cbind(Counts=tab,Percent=perc)
          else if(percentage)
            perc
          else
            tab
            ,class="table")
      })

setMethod("Table",signature(x="item.vector"),
    function(x,
            weights=NULL,
            counts=TRUE,
            percentage=(style=="codebook"),
            style=c("table","codebook","nolabels"),
            include.missings=(style=="codebook"),
            missing.marker=if(style=="codebook") "M" else "*",
            ...){
      if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
      is.m <- is.missing(x)
      isNA <- is.na(x)
      style <- match.arg(style)
      if (style %in% c("table","codebook")) {
        vl <- labels(x)
        if(length(vl)){
          vvl <- vl@values
          lvl <- vl@.Data
          valid <- !is.missing2(vvl,x@value.filter)
          i <- match(x@.Data,vvl,nomatch=0L)
          if(!length(weights)){
            tab <- tabulate(i,nbins=length(vvl))
            names(tab) <- as.character(vl@values)
            }
          else {
            f <- factor(x@.Data,levels=vvl)
            good <- is.finite(weights) & is.finite(f)
            tmp <- rowsum(weights[good],f[good])
            tab <- structure(rep(0,nlevels(f[good])),names=levels(f))
            tab[rownames(tmp)] <- tmp[]
          }
          lab <- if(style=="codebook") sQuote(vl@.Data) else vl@.Data
        }
        else {
          valid <- logical(0)
          tab <- c()
          lab <- c()
          i <- logical(length(x))
        }
        if(!length(weights)){
          ovld <- sum(!is.m & !i)
          omiss <- sum(is.m & !i & !isNA)
          NAs <- sum(isNA)
        }
        else {
          good <- is.finite(weights)
          weights <- weights[good]
          is.m <- is.m[good]
          i <- i[good]
          isNA <- isNA[good]
          ovld <- sum(weights*(!is.m & !i))
          omiss <- sum(weights*(is.m & !i & !isNA))
          NAs <- sum(weights*(isNA))
        }
        #browser()
        if(ovld){
          tab <- c(tab," "=ovld)
          if(style=="codebook")
            lab <- c(lab,"(unlab.val.)")
          else {
            lab <- if(length(vl)) c(lab,"Other valid") else c(lab,"Valid")
          }
          valid <- c(valid,TRUE)
          }
        if(include.missings){
          if(omiss){
            tab <- c(tab," "=omiss)
            if(style == "codebook")
              lab <- c(lab,"(unlab.mss.)")
            else {
              if(length(vl)){
                lab <- c(lab,"Other missing")
              } else {
                lab <- c(lab,"Missing")
                missing.marker <- ""
              }
            }
            valid <- c(valid,FALSE)
            }
          if(NAs){
            tab <- c(tab,"NA"=NAs)
            if(style == "codebook")
              lab <- c(lab,"")
            else {
                lab <- c(lab,"NA")
              if(!length(vl)){
                missing.marker <- ""
              }
            }
            valid <- c(valid,FALSE)
            }
          if(length(missing.marker)){
            missing.marker <- missing.marker[1]
            if(style=="codebook"){
              valid.marker <- paste(rep(" ",nchar(missing.marker)),collapse="")
              lab <- paste(ifelse(valid,valid.marker,missing.marker),lab)
              names(tab) <- paste(format(names(tab),justify="right"),format(lab,justify="left"))
            }
            else {
              lab <- paste(ifelse(valid,"",missing.marker),lab,sep="")
              names(tab) <- lab
            }
          }
        } else {
          if(style=="codebook")
            names(tab) <- paste(format(names(tab),justify="right"),format(lab,justify="left"))
          else
            names(tab) <- lab
          tab <- tab[valid]
        }
      }
      else { # style == "nolabels"
        if(include.missings){
          if(!length(weights)){
            NAs <- sum(isNA)
            tab <- table(x@.Data)
          }
          else {
            good <- is.finite(weights)
            weights <- weights[good]
            NAs <- sum(weights*isNA)
            tab <- rowsum(weights,x@.Data[good])
          }
          if(NAs)
            tab <- c(tab,"NA"=NAs)
          if(length(missing.marker)){
            missing.marker <- missing.marker[1]
            valid <- !is.missing2(sort(unique(x@.Data)),x@value.filter)
            if(NAs)
              valid <- c(valid,FALSE)
            lab <- paste(ifelse(valid,"",missing.marker),names(tab),sep="")
            names(tab) <- lab
          }
        }
        else if(!length(weights)){
            tab <- table(x@.Data[!is.m])
          }
          else {
            good <- is.finite(weights) & !is.m
            weights <- weights[good]
            tab <- rowsum(weights,x@.Data[good])
          }
      }
      if(include.missings){
        if(percentage && counts) {
          vperc <- rep(NA,length(tab))
          vtab <- tab[valid]
          Nvalid <- sum(vtab)
          if(Nvalid) vperc[valid] <- 100 * vtab/Nvalid
          else vperc[valid] <- 0
          tperc <- 100 * tab/sum(tab)
          tab <- cbind(Counts=tab,Valid=vperc,Total=tperc)
          }
        else if(percentage) {
          vperc <- rep(NA,length(tab))
          vtab <- tab[valid]
          Nvalid <- sum(vtab)
          if(Nvalid) vperc[valid] <- 100 * vtab/Nvalid
          else vperc[valid] <- 0
          tperc <- 100 * tab/sum(tab)
          tab <- cbind(Valid=vperc,Total=tperc)
          rownames(tab) <- names(tperc)
        }
      }
      else {
        if(percentage && counts) {
          perc <- 100 * tab/sum(tab)
          tab <- cbind(Counts=tab,Percent=perc)
          }
        else if(percentage) {
          tab <- 100 * tab/sum(tab)
        }
      }
     structure(tab,class="table")
})


wmean <- function(x,w){
    if(length(w)>0) weighted.mean(x,w)
    else mean(x)
}


NULLfunc <- function(...) NULL

Descriptives_numeric <- function(x,weights=NULL,...){

     isna <- is.na(x)
     if(length(weights)>0)
         w <- weights[!isna]
     else
         w <- NULL
     x <- x[!isna]
     m.1 <- tryCatch(wmean(1L*x,w),error=NULLfunc)
     x.cent <- tryCatch(x-m.1,error=NULLfunc)
     m.2 <- tryCatch(wmean(x.cent^2,w),error=NULLfunc)
     m.3 <- tryCatch(wmean(x.cent^3,w),error=NULLfunc)
     m.4 <- tryCatch(wmean(x.cent^4,w),error=NULLfunc)
     c(
      Min=min(x,na.rm=TRUE),
      Max=max(x,na.rm=TRUE),
      Mean=m.1,
      "Std.Dev."=tryCatch(sqrt(m.2),error=NULLfunc),
      Skewness=tryCatch(m.3/m.2^(3/2),error=NULLfunc),
      Kurtosis=tryCatch(m.4/m.2^2-3,error=NULLfunc)
     )
}

setMethod("Descriptives",signature(x="atomic"),
          Descriptives_numeric)

setMethod("Descriptives",signature(x="ANY"),
function(x,weights=NULL,...){
    if(mode(x)=="numeric")
        Descriptives_numeric(x,weights,...)
    else NULL
})


setMethod("Descriptives",signature(x="item.vector"),
    function(x,weights=NULL,...){
     miss <- is.missing(x)
     if(length(weights)>0)
         w <- weights[!miss]
     else
         w <- NULL
     x <- x@.Data[!miss]
     m.1 <- wmean(x,w)
     x.cent <- x-m.1
     m.2 <- wmean(x.cent^2,w)
     m.3 <- wmean(x.cent^3,w)
     m.4 <- wmean(x.cent^4,w)
     c(
      Min=min(x,na.rm=TRUE),
      Max=max(x,na.rm=TRUE),
      Mean=m.1,
      "Std.Dev."=sqrt(m.2),
      Skewness=m.3/m.2^(3/2),
      Kurtosis=m.4/m.2^2-3
     )
})

Moments <- function(x)
  c(
    mm.1 = mean(x),
    mm.2 = mean(x^2),
    mm.3 = mean(x^3),
    mm.4 = mean(x^4),
    N = length(x)
)

