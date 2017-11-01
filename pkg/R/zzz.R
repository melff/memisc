car_recode <- function (var, recodes, as.factor.result, levels)
  stop("package 'car' is not available")

memisc_env <- environment()


.onLoad <- function(lib,pkg){
  options(codebook.chunk.size=1000,
          Simulation.chunk.size=1000,
          print.use.value.labels=TRUE,
          show.max.obs=25)

  if(requireNamespace("car",quietly = TRUE)){
    assign("car_recode",car::recode,envir=memisc_env)
  }

  options(coef.style="default",
          show.baselevel=TRUE,
          baselevel.sep="/",
          factor.style="($f): ($l)",
          float.style="f",
          signif.symbols=c(
              "***"=.001,
              "**"=.01,
              "*"=.05
          ),
          labelled.factor.coerce.NA=FALSE,
          html.use.ampersand=FALSE,

          memisc.repr_html=TRUE,
          memisc.repr_latex=TRUE,
          mtable.always.eqnames=FALSE,

          signif.symbol.print.template=signif.symbol.print.default.template)
}


.onUnload <- function(libpath)
{
    library.dynam.unload("memisc", libpath)
}
