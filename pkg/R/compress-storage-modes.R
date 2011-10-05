compress.storage.mode <- function(x) UseMethod("compress.storage.mode")
compress.storage.mode.default <- function(x) x
compress.storage.mode.double <- function(x){
  ok <- is.finite(x)
  #y <- as.integer(x)
  if(max(abs(x[ok]))>.Machine$integer.max || any (x[ok]!=trunc(x[ok]))) x else as.integer(x)
}
