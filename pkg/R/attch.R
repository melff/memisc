include <- function(filename,warn=FALSE){
#     if(identical(parent.frame(),globalenv())){
      pos <- match(
              paste("source:",filename,sep=""),
              search()
              )
      if(is.finite(pos[1]))
          {
              if(warn) warning("\nReplacing source \"",filename,"\" at position ",pos)
              eval(substitute(detach(x),list(x=pos)))
          }
      env <- attach(NULL,name=paste("source:",filename,sep=""))
      exprs <- parse(n = -1, file = filename)
      for(i in exprs){
        eval(i,env,globalenv())
      }
      invisible(env)
#     }
#     else{
#       exprs <- parse(n = -1, file = filename)
#       env <- parent.frame()
#       for(i in exprs){
#         eval(i,env,globalenv())
#       }
#       invisible(parent.frame())
#       }
}


uninclude <- function(filename){
    pos <- match(
            paste("source:",filename,sep=""),
            search()
            )
    eval(substitute(detach(x),list(x=pos)))
}


detach.sources<- function(){
    pos <- grep("source:[A-Za-z.][A-Za-z0-9.]*",search())
    for(i in seq_along(pos))
      eval(substitute(detach(x),list(x=pos[i])))
}
