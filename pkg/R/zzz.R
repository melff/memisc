car_recode <- function (var, recodes, as.factor.result, levels)
  stop("package 'car' is not available")

memisc_env <- environment()


.onLoad <- function(lib,pkg){
  options(codebook.chunk.size=1000)
  options(Simulation.chunk.size=1000)
  options(print.use.value.labels=TRUE)
  options(show.max.obs=25)

  if(requireNamespace("car",quietly = TRUE)){
    assign("car_recode",car::recode,envir=memisc_env)
  }

  options(coef.style="default")
  options(show.baselevel=TRUE)
  options(baselevel.sep="/")
  options(factor.style="($f): ($l)")
  options(float.style="f")
  options(signif.symbols=c(
        "***"=.001,
        "**"=.01,
        "*"=.05
    ))
  options(labelled.factor.coerce.NA = FALSE)
  options(html.use.ampersand=FALSE)
}


.onUnload <- function(libpath)
{
    library.dynam.unload("memisc", libpath)
}
