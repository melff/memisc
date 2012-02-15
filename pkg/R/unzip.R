UnZip <- function(
  zipfile,
  item,
  dir=tempdir(),
  package=NULL
){


    if(length(package)){
      if(!suppressWarnings(require(package,character.only=TRUE)))
        stop(gettextf("there is no package called '%s'",package,domain="R-base"))
      zipfile <- file.path(system.file(package=package),zipfile)
    }
    utils::unzip(zipfile=zipfile,files=item,exdir=dir)
}