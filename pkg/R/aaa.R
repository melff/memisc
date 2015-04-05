
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

.CoefTemplates$ci.horizontal<- matrix(c(est="($est:#)",
                                           lwr="[($lwr:#)",
                                           upr="($upr:#)]"),
                                      ncol=3,nrow=1,byrow=TRUE,
                                      dimnames=list(
                                        c("est"),
                                        c("est","lwr","upr")
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
          "Aldrich-Nelson R-sq." = "($Aldrich.Nelson:f#)",
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
          "Aldrich-Nelson R-sq." = "($Aldrich.Nelson:f#)",
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


.SummaryTemplates$mer <- .SummaryTemplates$lmer
  c(
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )


assign("SummaryTemplates",.SummaryTemplates, envir=.memiscEnv)
assign("CoefTemplates",.CoefTemplates, envir=.memiscEnv)
 
