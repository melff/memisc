# There are of course more efficient ways to produce
# a random walk using the cusum() function. But this
# example shows how simple it is to
# conduct discrete event simulations using Simulate.
#
normal.random.walk <- Simulate({
    x <- x + rnorm(1)
    y <- y + rnorm(1)
    c(x=x,y=y)
    },
    start = {
      x <- 0
      y <- 0
    },
    nsim = 1000,
    seed = 1
  )
normal.random.walk <- as.data.frame(normal.random.walk)

t1.random.walk <- Simulate({
    u <- rchisq(1,df=1)
    delta.x <- rnorm(1)/u
    delta.y <- rnorm(1)/u
    x <- x + delta.x
    y <- y + delta.y
    c(x=x,y=y)
    },
    start = {
      x <- 0
      y <- 0
    },
    nsim = 1000,
    seed = 1
  )
t1.random.walk <- as.data.frame(t1.random.walk)

t2.random.walk <- Simulate({
    u <- rchisq(1,df=2)
    delta.x <- rnorm(1)/u
    delta.y <- rnorm(1)/u
    x <- x + delta.x
    y <- y + delta.y
    c(x=x,y=y)
    },
    start = {
      x <- 0
      y <- 0
    },
    nsim = 1000,
    seed = 1
  )
t2.random.walk <- as.data.frame(t2.random.walk)

t3.random.walk <- Simulate({
    u <- rchisq(1,df=3)
    delta.x <- rnorm(1)/u
    delta.y <- rnorm(1)/u
    x <- x + delta.x
    y <- y + delta.y
    c(x=x,y=y)
    },
    start = {
      x <- 0
      y <- 0
    },
    nsim = 1000,
    seed = 1
  )
t3.random.walk <- as.data.frame(t3.random.walk)

oldpar <- par(mfrow=c(2,2),ask=FALSE)
plot(normal.random.walk,type="l",asp=1)
lines(rbind(c(x=0,y=0),normal.random.walk[1,]))
points(x=0,y=0,col="red",pch=19,cex=1)
title("Normal random walk")

plot(t1.random.walk,type="l",asp=1)
lines(rbind(c(x=0,y=0),t1.random.walk[1,]))
points(x=0,y=0,col="red",pch=19,cex=1)
title("Student random walk, df = 1")

plot(t2.random.walk,type="l",asp=1)
lines(rbind(c(x=0,y=0),t2.random.walk[1,]))
points(x=0,y=0,col="red",pch=19,cex=1)
title("Student random walk, df = 2")
plot(t3.random.walk,type="l",asp=1)
lines(rbind(c(x=0,y=0),t3.random.walk[1,]))
points(x=0,y=0,col="red",pch=19,cex=1)
title("Student random walk, df = 3")
par(oldpar)
