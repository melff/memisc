
.memiscEnv <- new.env()
.SummaryTemplates <- list()
.CoefTemplates <- list()
.CoefTemplates$default <- c(est="($est:#)($p:*)",
                                          se="(($se:#))")
.CoefTemplates$stat <- c(est="($est:#)($p:*)",
                                      stat="(($stat:#))")
.CoefTemplates$all <- c(est="($est:#)($p:*)",
                                      se="(($se:#))",
                                      stat="(($stat:#))",
                                      p="(($p:#))"
                                      )
.CoefTemplates$all.nostar <- c(est="($est:#)",
                                      se="(($se:#))",
                                      stat="(($stat:#))",
                                      p="(($p:#))"
                                      )
.CoefTemplates$horizontal <- t(c(est="($est:#)($p:*)",
                                          se="(($se:#))"))
# .CoefTemplates$ci.se <- c(est="($est:3)",
#                                       se="(($se:#))",
#                                       ci="[($lwr:#);($upr:#)]")

.CoefTemplates$ci <- c(est="($est:#)",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )


.CoefTemplates$ci.se <- c(est="($est:#)",
                                        se="(($se:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )

.CoefTemplates$ci.se.horizontal<- matrix(c(est="($est:#)",
                                        se="(($se:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        ),ncol=2,nrow=2,byrow=TRUE,
                                        dimnames=list(
                                          c("est","ci"),
                                          c("est","se")
                                          ))

.CoefTemplates$ci.p <- c(est="($est:#)",
                                        p="(($p:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )

.CoefTemplates$ci.p.horizontal<- matrix(c(est="($est:#)",
                                        p="(($p:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        ),ncol=2,nrow=2,byrow=TRUE,
                                        dimnames=list(
                                          c("est","ci"),
                                          c("est","se")
                                          ))


.SummaryTemplates$lm <-
  c(
          "R-squared"     = "($r.squared:f#)",
          "adj. R-squared" = "($adj.r.squared:f#)",
          sigma         = "($sigma:#)",
          F             = "($F:f#)",
          p             = "($p:f#)",
          "Log-likelihood"  = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )

.SummaryTemplates$glm <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          phi         = "($phi:#)",
          "Likelihood-ratio" = "($LR:f#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )

.SummaryTemplates$default <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          "Likelihood-ratio" = "($LR:f#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )


assign("SummaryTemplates",.SummaryTemplates, env=.memiscEnv)
assign("CoefTemplates",.CoefTemplates, env=.memiscEnv)

sampleGeneric <- function(x, size, replace = FALSE, prob=NULL,...)
  UseMethod("sample")
environment(sampleGeneric) <- baseenv()
sample.default <- base::sample
formals(sample.default) <- formals(sampleGeneric)
environment(sample.default) <- baseenv()

car_recode <- function (var, recodes, as.factor.result, levels)
  stop("package 'car' is not available")

car_pkg <-"car"
memisc_env <- environment()


.onLoad <- function(lib,pkg){
  options(codebook.chunk.size=1000)
  options(Simulation.chunk.size=1000)
  options(print.use.value.labels=TRUE)
  options(show.max.obs=25)
  require(utils)
  require(stats)

  if(any(car_pkg == .packages(TRUE))){
    do.call("require",list(package=car_pkg))
    car_recode <- getFromNamespace("recode",ns=car_pkg)
    assign("car_recode",car_recode,env=memisc_env)
  }


  options(coef.style="default")
  options(baselevel.sep="/")
  options(factor.style="($f): ($l)")
  options(float.style="f")
  options(signif.symbols=c(
        "***"=.001,
        "**"=.01,
        "*"=.05
    ))
  options(labelled.factor.coerce.NA = FALSE)
  assignInNamespace(".sample.orig",base::sample , ns = "base")
  assignInNamespace("sample.default",sample.default , ns = "base")
  assignInNamespace("sample",sampleGeneric , ns = "base")
}


.onUnload <- function(libpath)
{
    assignInNamespace("sample",  base::.sample.orig,  ns = "base")

    library.dynam.unload("memisc", libpath)
}
