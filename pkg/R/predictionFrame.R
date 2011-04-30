add.residuals <- function(Y,object,variables,residuals=c("deviance","pearson","working")){
  residuals <- match.arg(residuals)
  r <- residuals(object,type=residuals)
  sapply(variables,function(v){
      Y[,paste(v,"fit",sep=".")] + r
      })
}

prediction.frame <- function(object,newdata=NULL,...,residuals=c("none","deviance","pearson","working",
  "standardized","studentized"))
  UseMethod("prediction.frame")

prediction.frame.default <- function(object,newdata=NULL,...,residuals=c("none","deviance","pearson","working",
  "standardized","studentized")){
  residuals <- match.arg(residuals)
  if(missing(newdata))
   {
    envir <- attr(formula(object),".Environment")
    vars <- all.vars(formula(object))
    X <- eval(object$call$data, envir)
    if(is.null(X)){
      X <- as.data.frame(lapply(vars,function(v)
              get(v,envir=envir)
            ))
      names(X) <- vars
    }
    else
      X <- as.data.frame(X)[vars]
    na.action <- object$na.action
    if(length(na.action))
      X <- X[-na.action,,drop=FALSE]
  }
    #X <- model.frame(object)
  else
    X <- newdata
  termLabels <- attr(terms(object),"term.labels")
  variables <- all.vars(delete.response(terms(object)))
  dots.arg <- list(...)
  type.is.variables <- as.logical(length(dots.arg$type)) && dots.arg$type=="variables"
  type.is.terms <- as.logical(length(dots.arg$type)) && dots.arg$type=="terms"
  tvLabels <- if(type.is.variables) variables else termLabels
  
  Y <- predict(object,newdata=newdata,...)
#   X <- X[variables]
  
  if(length(Y))
  Ynames <- names(Y)
  if(is.atomic(Y)){
    if(!length(dim(Y))){
      Y <- data.frame(prediction=Y)
    } else {
      Y <- as.data.frame(Y)
    }
    i <- names(Y) %in% tvLabels
    names(Y)[i] <- paste(names(Y)[i],"fit",sep=".")
  }
  else {
    Y <- lapply(seq(along=Y),function(i){
        if(!is.array(Y[[i]])){
          if(length(Y[[i]]) == nrow(X)){
          res <- data.frame(Y[[i]])
          return(res)
          }
          else
            return(NULL)
        }
        else {
          namesY.i <- names(Y)[i]
          res <- as.data.frame(Y[[i]])
          if(nrow(res)==nrow(X)){
            i <- names(res) %in% tvLabels
            names(res)[i] <- paste(names(res)[i],namesY.i,sep=".")
            return(res)
            }
          else
            return(NULL)
        }
      })
    getit <- !sapply(Y,is.null)
    Y <- Y[getit]
    #Ynames <- Ynames[getit]
    #names(Y) <- Ynames
    #browser()
    Y <- do.call("cbind",Y)
  }
  if(residuals=="none")
    return(cbind(X,Y))
  else {
    if(!missing(newdata)) stop("residuals not possible for new data")
    if(type.is.variables){
      if(residuals %in% c("standardized","studentized"))
        stop("partial residuals of type ",sQuote(residuals)," not supported")
      R <- add.residuals(Y,object,variables,residuals)
      }
    else if(residuals %in% c("working","pearson","deviance"))
      R <- do.call("residuals",c(list(object),type=residuals))
    else if(residuals == "standardized")
      R <- do.call("rstandard",list(object))
    else if(residuals == "studentized")
      R <- do.call("rstudent",list(object))
    if(!is.array(R))
      R <- data.frame(resid=R)
    else{
      R <- as.data.frame(R)
      i <- names(R) %in% tvLabels
      names(R)[i] <- paste(names(R)[i],"resid",sep=".")
      }
    if(length(Y) && ncol(R) && nrow(R))
        return(cbind(X,Y,R))
    else if (length(Y))
        return(cbind(X,Y))
    else if (ncol(R) && nrow(R))
        return(cbind(X,R))
    else return(X)
  }
}
