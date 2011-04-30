UnZip <- function(
  zipfile,
  item,
  dir=tempdir(),
  package=NULL
){

  if(getRversion() < "2.9.0"){
    cl <- match.call()
    filename <- NULL
    env <- environment()
    if(length(package)){
      if(!suppressWarnings(require(package,character.only=TRUE)))
        stop(gettextf("there is no package called '%s'",package,domain="R-base"))
      zipfile <- file.path(system.file(package=package),zipfile)
    }
    if (file.exists(zipfile)) {
        condition <- NULL
        rc <- tryCatch(.Internal(int.unzip(zipfile,item, dir,FALSE)),
            error=function(e) {
              assign("condition",e,env)
              TRUE
              },
            warning=function(e) {
              assign("condition",e,env)
              TRUE
              })
        if(rc){
          zipf <- gsub(".zip","",basename(zipfile),fixed=TRUE)
          item <- file.path(zipf,item)
          rc <- tryCatch(.Internal(int.unzip(zipfile,item, dir,FALSE)),
              error=function(e) TRUE,
              warning=function(e) TRUE)
        }
        if(rc && length(condition)) stop(simpleError(gettext(conditionMessage(condition)),call=cl))
        filename <- file.path(dir, item)
    } else stop(gettextf("file '%s' not found",zipfile,domain="R-base"))
    filename
  }
  else {

    if(length(package)){
      if(!suppressWarnings(require(package,character.only=TRUE)))
        stop(gettextf("there is no package called '%s'",package,domain="R-base"))
      zipfile <- file.path(system.file(package=package),zipfile)
    }
    utils::unzip(zipfile=zipfile,files=item,exdir=dir)
  }
}