setMethod("wild.codes","item",function(x){
  vl <- labels(x)
  vvl <- vl@values
  use <- !(x %in% vvl)
  N <- length(x)
  x <- x@.Data[use]
   
  tab <- Table(x,counts=TRUE,percentage=TRUE)
  tab[,2] <- 100*tab[,1]/N
  tab
})