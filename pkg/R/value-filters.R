## value filters #######################################################################

setAs(from="atomic",to="valid.values",function(from,to)new(to,filter=from))
setAs(from="atomic",to="valid.range",function(from,to)new(to,filter=range(from)))
setAs(from="atomic",to="missing.values",function(from,to)new(to,filter=from))
setAs(from="list",to="missing.values",function(from,to)new(to,filter=from$values,range=from$range))

is.valid2 <- function(x,filter){
  is.na.x <- is.na(x)
  if(!length(filter)) !is.na.x
  else switch(class(filter),
      valid.values = !is.na.x & (x %in% filter@filter),
      valid.range = !is.na.x & (x >= filter@filter[1] & x <= filter@filter[2]),
      missing.values = {
          vld <- !is.na.x
          if(length(filter@filter)) vld <- vld & (x %nin% filter@filter)
          if(length(filter@range)==2) vld <- vld & (x < filter@range[1] | x > filter@range[2])
          vld
        }
      )
}

is.missing2 <- function(x,filter){
  is.na.x <- is.na(x)
  if(!length(filter)) is.na.x
  else switch(class(filter),
      valid.values = is.na.x | x %nin% filter@filter,
      valid.range = is.na.x | (x < filter@filter[1] | x > filter@filter[2]),
      missing.values = {
          msng <- is.na.x 
          if(length(filter@filter)) msng <- msng | x %in% filter@filter
          if(length(filter@range)==2) msng <- msng | (x >= filter@range[1] & x <= filter@range[2])
          msng
        }
      )
}

setMethod("value.filter",signature(x="item"),function(x)x@value.filter)

setMethod("value.filter<-",signature(x="item",value="NULL"),
  function(x,value){
    x@value.filter <- NULL
    x
})

setMethod("value.filter<-",signature(x="item",value="value.filter"),
  function(x,value){
    x@value.filter <- value
    x
})

setMethod("valid.values",signature(x="item.vector"),function(x){
  filter <- x@value.filter
  if(!length(filter)) return(new("valid.values",filter=sort(unique(x@.Data))))
  vals <- switch(class(filter),
    valid.values=return(filter),
    valid.range=sort(unique(x@.Data[x >= filter@filter[1] & x <= filter@filter[2]])),
    missing.values=sort(unique(x@.Data[!is.missing2(x,filter)]))
    )
  new("valid.values",filter=vals)
})

setMethod("missing.values",signature(x="item.vector"),function(x){
  filter <- x@value.filter
  if(!length(filter)) return(NULL)
  vals <- switch(class(filter),
    valid.values=sort(unique(x@.Data[x %nin% filter@filter])),
    valid.range=sort(unique(x@.Data[x < filter@filter[1] | x > filter@filter[2]])),
    missing.values=return(filter)
    )
  new("missing.values",filter=vals)
})

setMethod("valid.range",signature(x="item.vector"),function(x){
  filter <- x@value.filter
  if(!length(filter)) return(new("valid.range",filter=range(range(x@.Data))))
  vals <- switch(class(filter),
    valid.values=range(x[x %in% filter@filter]),
    valid.range=return(filter),
    missing.values={
        rgn <- range(x[!is.missing2(x,filter)])
        mv <- filter@filter
        if(any(mv >= rgn[1] & mv <= rgn[2])) stop("cannot make a valid range from this missing values definition")
        rgn
      }
    )
  new("valid.range",filter=range(vals))
})


setReplaceMethod("missing.values",signature(x="item",value="NULL"),
  function(x,value){
    x@value.filter <- NULL
    x
})

setReplaceMethod("missing.values",signature(x="ANY",value="atomic"),
  function(x,value){
    missing.values(x) <- new("missing.values",filter=value)
    x
  })
setReplaceMethod("missing.values",signature(x="ANY",value="list"),
  function(x,value){
    missing.values(x) <- new("missing.values",filter=value$values,range=value$range)
    x
  })
setReplaceMethod("missing.values",signature(x="item",value="missing.values"),
  function(x,value){
    x@value.filter <- value
    x
})
setReplaceMethod("missing.values",signature(x="atomic",value="missing.values"),
  function(x,value){
    if(is(x,"item")){
      message("'missing.values<-' called with item as atomic")
      x@value.filter <- value
      x
    }
    else as.item(x,value.filter=value)
})


setReplaceMethod("valid.values",signature(x="ANY",value="atomic"),
  function(x,value){
    valid.values(x) <- new("valid.values",filter=value)
    x
  })
setReplaceMethod("valid.values",signature(x="item",value="valid.values"),
  function(x,value){
    x@value.filter <- value
    x
})
setReplaceMethod("valid.values",signature(x="atomic",value="valid.values"),
  function(x,value){
    if(is(x,"item")){
      message("'valid.values<-' called with item as atomic")
      x@value.filter <- value
      x
    }
    else as.item(x,value.filter=value)
})

setReplaceMethod("valid.range",signature(x="ANY",value="atomic"),
  function(x,value){
    valid.range(x) <- new("valid.range",filter=value)
    x
  })
setReplaceMethod("valid.range",signature(x="item",value="valid.range"),
  function(x,value){
    x@value.filter <- value
    x
})
setReplaceMethod("valid.range",signature(x="atomic",value="valid.range"),
  function(x,value){
    if(is(x,"item")){
      message("'valid.range<-' called with item as atomic")
      x@value.filter <- value
      x
    }
    else as.item(x,value.filter=value)
})



setMethod("Arith",signature(e1="valid.values",e2="valid.values"),
  function(e1,e2){
    new("valid.values",filter=switch(.Generic,
      "+" = union(e1@filter,e2@filter),
      "-" = setdiff(e1@filter,e2@filter),
      stop("unsupported operator ",dQuote(.Generic))
    ))
})

setMethod("Arith",signature(e1="missing.values",e2="missing.values"),
  function(e1,e2){
    new("missing.values",filter=switch(.Generic,
      "+" = union(e1@filter,e2@filter),
      "-" = setdiff(e1@filter,e2@filter),
      stop("unsupported operator ",dQuote(.Generic))
    ))
})

setMethod("Arith",signature(e1="valid.range",e2="valid.range"),
  function(e1,e2){
    r1 <- e1@filter
    r2 <- e2@filter
    if(r1[2] < r2[1] || r1[1] > r2[2]) warning("ranges leave a gap")
    new("valid.values",filter=switch(.Generic,
      "+" = range(r1,r2),
      stop("unsupported operator ",dQuote(.Generic))
    ))
})

setMethod("Arith",signature(e1="value.filter",e2="vector"),
  function(e1,e2) callGeneric(e1,as(e2,class(e1))))

setMethod("is.missing",signature(x="atomic"),function(x){
  is.na(x)
})

setMethod("is.missing",signature(x="factor"),function(x){
  is.na(x)
})


setMethod("is.missing",signature(x="item.vector"),function(x){
  filter <- x@value.filter
  is.missing2(x,filter)
})

is.valid <- function(x) !is.missing(x) & !is.na(x)
nvalid <- function(x) sum(is.valid(x))


format.valid.values <- function(x,...){
  paste(as.character(x@filter),collapse=", ")
}
format.valid.range <- function(x,...){
  paste(as.character(x@filter[1:2]),collapse="-")
}
format.missing.values <- function(x,...){
  if(length(x@filter) && length(x@range))
    paste(
      paste(as.character(x@filter),collapse=", "),
      paste(as.character(x@range[1:2]),collapse="-"),
      sep=", ")
  else if(length(x@filter))
    paste(as.character(x@filter),collapse=", ")
  else if(length(x@range))
    paste(as.character(x@range[1:2]),collapse="-")
}

# setMethod("format","valid.values",format.valid.values) 
# setMethod("format","valid.range",format.valid.range) 
# setMethod("format","missing.values",format.missing.values) 


setMethod("show","value.filter",function(object){
  writeLines(format(object))
})

setMethod("include.missings","item",function(x,mark="*"){
  if(length(vl <- x@value.labels)){
    vlv <- vl@values
    labs <- vl@.Data
    ism <- is.missing2(vlv,x@value.filter)
    labs[ism] <- paste(mark,labs[ism],sep="")
    vl@.Data <- labs
    x@value.labels <- vl
  }
  x@value.filter <- NULL
  x
})

