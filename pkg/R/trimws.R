trimws <- function(x,left=TRUE,right=TRUE){
  res <- x
  if(left)
    res <- sub('^[ \t\r\n]+', '',res)
  if(right)
    res <- sub('[ \t\r\n]+$', '',res)
  res
}

