t.normal.simres <- Simulate(
  start = {
    if(df==Inf) rtnorm <- function(n) rnorm(n)
    else rtnorm <-  function(n) rt(n,df)
  },
  step = {
    x <- rtnorm(n)
    list(
      mean = mean(x),
      sd = sd(x),
      median = median(x),
      mad = mad(x)
      )
  },
  conditions = expand.grid(
      n=c(10,20,50,100,200,500,1000),
      df=c(Inf,5,3,2,1)
    ),
  nsim = 100,
  trace=25,
  seed=4
)

t.normal.simres <- as.data.frame(t.normal.simres)
t.normal.simres <- within(t.normal.simres,{
  df <- factor(df,levels=unique(df))
  n <- factor(n)
  })

t.normal.mean <- aggregate(
              c(
                mean=mean(mean),
                sd=sd(mean)
              )~df+n,
          data=t.normal.simres,sort=TRUE)

t.normal.stddev <- aggregate(
              c(
                mean=mean(sd),
                sd=sd(sd)
              )~df+n,
          data=t.normal.simres,sort=TRUE)

t.normal.median <- aggregate(
              c(
                mean=mean(median),
                sd=sd(median)
              )~df+n,
          data=t.normal.simres,sort=TRUE)

t.normal.mad <- aggregate(
              c(
                mean=mean(mad),
                sd=sd(mad)
              )~df+n,
          data=t.normal.simres,sort=TRUE)

t.normal.sumry <- collect(
  t.normal.mean,
  t.normal.stddev,
  t.normal.median,
  t.normal.mad,
  names=c("mean","stddev","median","mad"),
  sourcename="stat"
  )
          
t.normal.sumry <- within(t.normal.sumry,
          type <- relabel(df,
            "Inf"="Normal",
            "5"="Student's t, df=5",
            "3"="Student's t, df=3",
            "2"="Student's t, df=2",
            "1"="Student's t, df=1"
            )
          )

if(interactive())
  old.prompt <- devAskNewPage(TRUE)

xyplot(cbind(mean,mean+2*sd,mean-2*sd)~n|type,
  data=subset(t.normal.sumry,
        subset=(stat=="mean")),
  main="Mean",
  ylab="Mean of 100 replications +/- 2 standard deviations",
  xlab="Sample size",
  panel=panel.errbars,
  scales=list(y="free"),
  as.table=TRUE,
  type="o",
  pch=19)

xyplot(cbind(mean,mean+2*sd,mean-2*sd)~n|type,
  data=subset(t.normal.sumry,
        subset=(stat=="stddev")),
  main="Standard deviation",
  ylab="Mean of 100 replications +/- 2 standard deviations",
  xlab="Sample size",
  panel=panel.errbars,
  scales=list(y="free"),
  as.table=TRUE,
  type="o",
  pch=19)

xyplot(cbind(mean,mean+2*sd,mean-2*sd)~n|type,
  data=subset(t.normal.sumry,
        subset=(stat=="median")),
  main="Median",
  ylab="Mean of 100 replications +/- 2 standard deviations",
  xlab="Sample size",
  panel=panel.errbars,
  scales=list(y="free"),
  as.table=TRUE,
  type="o",
  pch=19)

xyplot(cbind(mean,mean+2*sd,mean-2*sd)~n|type,
  data=subset(t.normal.sumry,
        subset=(stat=="mad")),
  main="Median absolute deviation",
  ylab="Mean of 100 replications +/- 2 standard deviations",
  xlab="Sample size",
  panel=panel.errbars,
  scales=list(y="free"),
  as.table=TRUE,
  type="o",
  pch=19)

if(interactive())
  devAskNewPage(old.prompt)

