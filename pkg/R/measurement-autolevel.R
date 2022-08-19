setGeneric("measurement_autolevel",function(x,...)standardGeneric("measurement_autolevel"))

setMethod("measurement_autolevel",
          signature(x="ANY"),function(x,...)x)

# If the most values are labelled, set the measurement level to "nominal" or "ordinal".
setMethod("measurement_autolevel",
          signature(x="item.vector"),
          function(x,
                   to=getOption("measurement.adapt.default","nominal"),
                   threshold=getOption("measurement.adapt.threshold",.75),
                   ...){
    mnt <- measurement(x)
    lab <- labels(x)
    if(!length(lab) || mnt%in%c("nominal","ordinal"))
        return(x)
    else {
        is_labd <- x %in% lab@values
        is_vald <- is.valid(x)
        is_labd_n_vald <- is_labd & is_vald
        prop_labd <- sum(is_labd_n_vald)/sum(is_vald)
        if(prop_labd > threshold)
            measurement(x) <- to
        return(x)
    }
})

setMethod("measurement_autolevel",
          signature(x="data.set"),
          function(x,
                   to=getOption("measurement.adapt.default","nominal"),
                   threshold=getOption("measurement.adapt.threshold",.75),
                   except=NULL,
                   only=NULL,
                   ...){
  nms <- names(x)
  if(!missing(except)){
      except <- substitute(except)
      if(length(except)>1)
          except <- sapply(except[-1],as.character)
      else
          except <- as.character(except)
      nms <- setdiff(nms,except)
  }
  if(!missing(only)){
      only <- substitute(only)
      if(length(only)>1)
          only <- sapply(only[-1],as.character)
      else
          only <- as.character(only)
      nms <- intersect(nms,only)
  }
  for(n in nms){
      x[[n]] <- measurement_autolevel(x[[n]])
  }
  return(x)
})
