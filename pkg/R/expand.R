expand <- function(df,w){
  w <- eval(substitute(w),df,parent.frame())
  i <- seq(nrow(df))
  i <- rep(i,as.integer(w))
  df[i,]
}


# test.expand <- function(){
#   testdata <- data.frame(x=c(1,2,3),y=c(4,5,6),weights=c(4,8,12))
#   print(testdata)
#   print(
#     expand(testdata,weights)
#     )
# }
# 
# test.expand()