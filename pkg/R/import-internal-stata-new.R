dta117_file_open <- function(file) .Call("dta117_file_open",file,"rb")
dta117_file_close <- function(file) .Call("dta117_file_close",file)
dta117_check_magic <- function(ptr) .Call("dta117_check_magic",ptr)
dta117_read_header <- function(ptr) .Call("dta117_read_header",ptr)
dta117_read_map <- function(ptr) .Call("dta117_read_map",ptr)
dta117_read_vtypes <- function(ptr) .Call("dta117_read_vtypes",ptr)
dta117_read_vnames <- function(ptr) .Call("dta117_read_vnames",ptr)
dta117_read_sortlist <- function(ptr) .Call("dta117_read_sortlist",ptr)
dta117_read_formats <- function(ptr) .Call("dta117_read_formats",ptr)
dta117_read_vlab_names <- function(ptr) .Call("dta117_read_vlab_names",ptr)
dta117_read_varlabs <- function(ptr) .Call("dta117_read_varlabs",ptr)
dta117_read_vallabs <- function(ptr) .Call("dta117_read_vallabs",ptr)
dta117_make_prototype <- function(types) .Call("dta117_make_prototype",types)
dta117_dim  <- function(types) .Call("dta117_dim",types)
dta117_seek_data <- function(ptr) .Call("dta117_seek_data",ptr)
dta117_read_data <- function(ptr,what,n,types) .Call("dta117_read_data",ptr,what,n,types)
dta117_read_slice <- function(ptr,what,vars,obs,types) .Call("dta117_read_slice",
                                                             ptr,what,vars,obs,types)
dta117_read_chunk <- function(ptr,what,vars,n,types) .Call("dta117_read_chunk",
                                                           ptr,what,vars,n,types)

dta117.byte   <- 65530
dta117.short  <- 65529
dta117.long   <- 65528
dta117.float  <- 65527
dta117.double <- 65526

dta117.byte.missrange   <- c(0x64,0x7f)
dta117.short.missrange  <- c(0x7fe5,0x7fff)
dta117.long.missrange   <- c(0x7fffffe5L,0x7fffffffL)
dta117.float.missrange  <- c(2.0^0x7f,Inf)
dta117.double.missrange <- c(2.0^0x3ff,Inf)

dta117.missnames <- paste(".",c("",letters),sep="")
dta117.byte.misslab <- structure(0x64 + 1:27,names=dta117.missnames)
dta117.short.misslab <- structure(0x7fe4 + 1:27,names=dta117.missnames)
dta117.long.misslab <- structure(0x7fffffe4L + 1:27,names=dta117.missnames)
dta117.float.misslab <- structure((1+(0:26)/16^3)*2.0^0x7f,names=dta117.missnames)
dta117.double.misslab <- structure((1+(0:26)/16^3)*2.0^0x3ff,names=dta117.missnames)

dta117_missing.values <- function(types){
    nvar <- length(length)
    missing.values <- vector(nvar,mode="list")
    missing.values[types==dta117.byte]   <- list(list(range=dta117.byte.missrange))
    missing.values[types==dta117.short]  <- list(list(range=dta117.short.missrange)) 
    missing.values[types==dta117.long]   <- list(list(range=dta117.long.missrange))
    missing.values[types==dta117.float]  <- list(list(range=dta117.float.missrange)) 
    missing.values[types==dta117.double] <- list(list(range=dta117.double.missrange))
    names(missing.values) <- names(types)
    missing.values
}

dta117_missval_labels <- function(types){
    nvar <- length(length)
    missval_labels <- vector(nvar,mode="list")
    missval_labels[types==dta117.byte]   <- list(dta117.byte.misslab)
    missval_labels[types==dta117.short]  <- list(dta117.short.misslab) 
    missval_labels[types==dta117.long]   <- list(dta117.long.misslab)
    missval_labels[types==dta117.float]  <- list(dta117.float.misslab)
    missval_labels[types==dta117.double] <- list(dta117.double.misslab)
    names(missval_labels) <- names(types)
    missval_labels
}
