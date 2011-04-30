lm.example <- function(a=0,b=1,n=101,xrange=c(-1,1),serr=1){
  x <- seq(from=xrange[1],to=xrange[2],length=n)
  y <- a + b*x + rnorm(n,sd=serr)
  lm.res <- lm(y~x)
  coef <- lm.res$coef
  names(coef) <- c("a","b")
  coef
}

lm.simres <- Simulate(
      lm.example(n=n,serr=serr),
      expand.grid(
      serr=c(0.1,1,10),
      n=c(11,101,501)
      ),
      nsim=200,
      trace=50
    )
genTable(c(sd(a),sd(b))~serr+n,data=lm.simres)
