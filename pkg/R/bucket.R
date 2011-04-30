default_bucket <- function(size=1){
  bucket <- environment()
  with(bucket,{
    open <- TRUE
    size<-size
    nobs <- 0
    nvar <- 0
  })
  structure(bucket,class=c("default_bucket","bucket"))
}
# default.bucket <- function(size=NA){
#   init <- TRUE
#   i <- 1
#   e <- environment()
#   res <- function(){
#     print(init)
#     if(init) with(e,init <- FALSE)
#     with(e,i <- i+1)
#   }
#   structure(res,class="bucket")
# }


put_into <- function(bucket,value) UseMethod("put_into")
put_into.default_bucket <- function(bucket,value){
    if(!bucket$open) stop("bucket is closed",call.=FALSE)
    bucket$x <- unlist(value)
    with(bucket,{
      if(nobs==0){
          nvar <- length(x)
          Data <- matrix(NA,ncol=size,nrow=nvar)
          vnames <- rownames(Data) <- if(length(names(x))) names(x) else paste("var",seq(along=x),sep=".")
        }
      if(nobs > 0 && !(nobs%%size)){
        ncol.Data <- ncol(Data)+size
        length(Data) <- nvar*ncol.Data
        dim(Data) <- c(nvar,ncol.Data)
        rownames(Data) <- vnames
      }
      nobs <- nobs + 1
      Data[,nobs] <- x
    })
}
dim.default_bucket <- function(x) with(x,c(nobs,nvar))
as.matrix.default_bucket <- function(x,...) with(x,{
                                  if(nobs)
                                    t(Data[,1:nobs,drop=FALSE])
                                  else NULL
                                  })
as.data.frame.default_bucket <- function(x,...) as.data.frame(
                                    with(x,{
                                      if(nobs)
                                        t(Data[,1:nobs,drop=FALSE])
                                      else NULL
                                      }),...)
print.bucket <- function(x,...) {
  cat("A",sQuote(class(x)[1]),"object")
  cat(" with",ncol(x),"variables and",nrow(x),"observations\n")
}
pour_out <- function(bucket,...)UseMethod("pour_out")
pour_out.default_bucket <- function(bucket,...){
  with(bucket,{
    open <- FALSE
    #Data <- Data[,1:nobs,drop=FALSE]
  })
  bucket
}


textfile_bucket <- function(size=1,name=date()){
  bucket <- environment()
  with(bucket,{
    open <- TRUE
    size<-size
    f <- file(name,"w+")
    nobs <- 0
  })
  structure(bucket,class=c("textfile_bucket","bucket"))
}

put_into.textfile_bucket <- function(bucket,value){
    if(!bucket$open) stop("bucket is closed",call.=FALSE)
    bucket$x <- unlist(value)
    with(bucket,{
      if(nobs==0){
          nvar <- length(x)
          header <- if(length(names(x))) names(x) else paste("var",seq(along=x),sep=".")
          cat(header,file=f,sep="\t")
          cat("\n",file=f)
        }
      cat(paste(x),file=f,sep="\t")
      cat("\n",file=f)
      nobs <- nobs + 1
    })
}
pour_out.textfile_bucket <- function(bucket,...){
  with(bucket,{
    open <- FALSE
    close(f)
  })
  bucket
}
dim.textfile_bucket <- function(x) with(x,c(nobs,nvar))

as.data.frame.textfile_bucket <- function(x,...)
  read.table(x$name,header=TRUE)

as.matrix.textfile_bucket <- function(x,...)
  as.matrix(read.table(x$name,header=TRUE))

"[.bucket" <- function(x,i,...)as.data.frame(x)[i,]