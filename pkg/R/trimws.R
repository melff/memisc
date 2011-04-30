trimws <- function(x,left=TRUE,right=TRUE){
  res <- x
  old.locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE","C")
  if(left)
    res <- sub('^\\s+', '',res)
  if(right)
    res <- sub('\\s+$', '',res)
  Sys.setlocale("LC_CTYPE",old.locale)
  res
}

