panel.errbars1 <- function(x,y0,y1,ewidth=0){
  x <- as.numeric(x)
  offs <- ewidth/2
  panel.segments(x0=x,x1=x,y0=y0,y1=y1)
  panel.segments(x0=x-offs,x1=x+offs,y0=y0,y1=y0)
  panel.segments(x0=x-offs,x1=x+offs,y0=y1,y1=y1)
}

     
panel.errbars <- function(x,y,...,panel.xy=panel.xyplot,make.grid=c("horizontal","vertical","both","none"),ewidth=0){
        Y <- matrix(y,nrow=length(y)/3,ncol=3)
        y <- Y[,1]
        y0 <- Y[,2]
        y1 <- Y[,3]
        make.grid <- match.arg(make.grid)
        if(make.grid=="horizontal")
          panel.grid(h=-1,v=0)
        else if(make.grid=="vertical")
          panel.grid(v=-1,h=0)
        else if(make.grid=="both")
          panel.grid(v=-1,h=-1)
        panel.errbars1(x,y0,y1,ewidth=ewidth)       
        panel.xy(x,y,...)
      }
