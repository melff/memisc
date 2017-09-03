## Not exported yet ...

inflate <- function(df,w){
  w <- eval(substitute(w),df,parent.frame())
  i <- seq(nrow(df))
  i <- rep(i,as.integer(w))
  df[i,]
}


# test.inflate <- function(){
#   testdata <- data.frame(x=c(1,2,3),y=c(4,5,6),weights=c(4,8,12))
#   print(testdata)
#   print(
#     inflate(testdata,weights)
#     )
# }
# 
# test.expand()
