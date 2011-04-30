## Grid infrastructure ########################################################

newGrid <- function(nrow=8,ncol=8){
  e <- new.env()
  e$Data <- matrix(list(),ncol=ncol,nrow=nrow)
  rownames(e$Data) <- 1:nrow
  colnames(e$Data) <- 1:ncol
  structure(e,class="Grid")
  }

copyGrid <- function(x){
  e <- new.env()
  e$Data <- x$Data
  structure(e,class="Grid")
}

print.Grid <- function(x,...){
  cat("Grid with",length(agents(x)),"agents\n")
  matrix <- noquote(Sapply(x$Data,function(x)if(is.null(x))"" else x))
  print(matrix,...)
}

newGridAgent <- function(type,grid,row=NA,col=NA){
  nrow <- nrow(grid$Data)
  ncol <- ncol(grid$Data)
  if(is.na(row) && is.na(col)){
    free <- which(sapply(grid$Data,is.null))
    coords <- as.matrix(expand.grid(1:nrow,1:ncol))
    new.coords <- coords[sample(free,1),]
    row <- new.coords[1]
    col <- new.coords[2]
  }
  else if(is.na(row)){
    free <- sapply(grid$Data[,col],is.null)
    row <- sample(free,1)
  }
  else if(is.na(col)){
    free <- sapply(grid$Data[row,],is.null)
    col <- sample(free,1)
  }
  if(row > nrow || row < 1) stop("no row no. ",row," in grid")
  if(col > ncol || col < 1) stop("no column no. ",row," in grid")
  this <- structure(
    type,
    grid=grid,
    row=row,
    col=col,
    class="GridAgent"
  )
  grid$Data[[row,col]] <- this
  this
}

print.GridAgent <-function(x,...){
  cat("grid agent of type",sQuote(x),"at",paste("[",attr(x,"row"),",",attr(x,"col"),"]",sep=""),"\n")
}

agents <- function(grid){
  mat <- grid$Data
  mat[!sapply(mat,is.null)]
}

type <- function(x) UseMethod("type")
type.default <- function(x) ""
type.GridAgent <- function(x) as.character(x)

rowpos <- function(agent) attr(agent,"row")
colpos <- function(agent) attr(agent,"col")

plot.Grid <- function(x,col=c("white","blue","red"),...){
  nrow <- nrow(x$Data)
  ncol <- ncol(x$Data)
  xx <- 1:ncol
  yy <- 1:nrow
  zchar <- Sapply(x$Data,
          function(data){
            if(length(data)) type(data)
            else ""
            }
          )
  types <- sort(unique(c(zchar)))
  zz <- array(match(zchar,types)-1,dim=dim(zchar))
  zz <- t(zz[nrow:1,])
  image(xx,yy,zz,col=col,xlab="",ylab="",axes=FALSE,frame.plot=TRUE,mar=rep(0.1,4))
  box()
  axis(1,at=1:ncol,las=1)
  axis(2,at=1:nrow,las=1)
}

"[.Grid" <- function(x,i,j,...){
  i <- i[1]
  j <- j[1]
  structure(c(i,j),
    occupant = x$Data[[i,j]],
    grid = x,
    class = "GridPosition"
  )
}

print.GridPosition <- function(x,...){
  cat("cell",paste("[",x[1],",",x[2],"]",sep=""),"")
  if(length(attr(x,"occupant")))
    cat("occupied by",attr(x,"occupant"),"\n")
  else
    cat("unoccupied\n")
}

cell <- function(x){
  grid <- attr(x,"grid")
  row <- attr(x,"row")
  col <- attr(x,"col")
  grid[row,col]
}

occupant <- function(x)attr(x,"occupant")
occupied <- function(x)as.logical(length(attr(x,"occupant")))

moveto <- function(this,...) UseMethod("moveto")
moveto.GridAgent <- function(this,to){
  grid <- attr(this,"grid")
  if(!inherits(to,"GridPosition")) stop("'to' is not a position in a grid")
  if(!identical(grid,attr(to,"grid"))) stop("cannot move to different grid")
  row <- unclass(to)[1]
  col <- unclass(to)[2]
  nrow <- nrow(grid$Data)
  ncol <- ncol(grid$Data)
  if(row > nrow || row < 1) stop("no row no. ",row," in grid")
  if(col > ncol || col < 1) stop("no column no. ",row," in grid")
  if(!is.null(grid$Data[[row,col]]))
    stop("cell [",row,",",col,"] already occupied")
  from.row <- attr(this,"row")
  from.col <- attr(this,"col")
  grid$Data[from.row,from.col] <- list(NULL)
  attr(this,"row") <- row
  attr(this,"col") <- col
  grid$Data[[row,col]] <- this
}

neighborhood <- function(this,diagonal=TRUE,...) UseMethod("neighborhood")

neighborhood.GridPosition <- function(this,diagonal=TRUE,...) {
  grid <- attr(this,"grid")
  mat <- grid$Data
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  if(!is.matrix(this)) this <- t(as.matrix(this))
  else if(NROW(this)>1) warn("only first cell considered")
  row <- this[1,1]
  col <- this[1,2]
  nh <- col(mat) == col & row(mat) %in% c(row-1,row+1) | row(mat) == row & col(mat) %in% c(col-1,col+1)
  if(diagonal) nh <- nh | row(mat) %in% c(row-1,row+1) & col(mat) %in% c(col-1,col+1)
  coords <- as.matrix(expand.grid(1:nrow,1:ncol))[nh,]
  ans <- lapply(1:nrow(coords),function(i)grid[coords[i,1],coords[i,2]])
  structure(ans,grid=grid,class="GridNeighborhood")
}

neighborhood.GridAgent <- function(this,diagonal=TRUE,...) {
  grid <- attr(this,"grid")
  mat <- grid$Data
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  row <- attr(this,"row")
  col <- attr(this,"col")
  nh <- col(mat) == col & row(mat) %in% c(row-1,row+1) | row(mat) == row & col(mat) %in% c(col-1,col+1)
    if(diagonal) nh <- nh | row(mat) %in% c(row-1,row+1) & col(mat) %in% c(col-1,col+1)
  coords <- as.matrix(expand.grid(1:nrow,1:ncol))[nh,]
  ans <- lapply(1:nrow(coords),function(i)grid[coords[i,1],coords[i,2]])
  structure(ans,grid=grid,class="GridNeighborhood")
}

canMove <- function(this,diagonal=TRUE){
  nh <- neighborhood(this,diagonal=diagonal)
  !all(sapply(nh,occupied))
}

print.GridNeighborhood <- function(x,...){
  for(cell in x) print(cell,...)
}

summary.GridNeighborhood <- function(object,...){
  occupants <- lapply(object,occupant)
  grid <- attr(object,"grid")
  all.types <- union(sort(unique(sapply(agents(grid),type))),"")
  occupants <- sapply(occupants,type)
  table(factor(occupants,levels=all.types))
}

freecells <- function(grid){
  mat <- grid$Data
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  coords <- as.matrix(expand.grid(1:nrow,1:ncol))
  free <- sapply(mat,is.null)
  free <- coords[free,]
  lapply(1:nrow(free),function(i)grid[free[i,1],free[i,2]])
}

## Model assumptions ################################################################################

satisfied <- function(agent,threshold=.5){
  type <- as.character(agent)
  sum.nh <- summary(neighborhood(agent))
  occ.nh <- sum.nh[nzchar(names(sum.nh))]
  if(!sum(occ.nh)) return(FALSE)
  eq.nh <- occ.nh[type]
  prop.eq <- eq.nh/sum(occ.nh)
  return(prop.eq>=threshold)
}

dissatisfied.agents <- function(grid,threshold=.5) agents(grid)[!sapply(agents(grid),satisfied,threshold=.5)]

quality.cell <- function(cell,agent){
  type <- type(agent)
  sum.nh <- summary(neighborhood(cell))
  occ.nh <- sum.nh[nzchar(names(sum.nh))]
  occ.nh[names(occ.nh)==type] - occ.nh[names(occ.nh)!=type]
}

movetobest <- function(agent,diagonal=FALSE){
  nh <- neighborhood(agent,diagonal=diagonal)
  nh <- nh[!sapply(nh,occupied)]
  if(!length(nh)) return(FALSE)
  quality.nh <- sapply(nh,quality.cell,agent=agent) - 1
  quality.own <- quality.cell(cell(agent),agent)
  if(length(quality.nh) && quality.nh > quality.own){
    best <- which(quality.nh == max(quality.nh))
    best <- nh[[sample(best,1)]]
    moveto(agent,best)
    return(TRUE)
    }
  return(FALSE)
}

jumptobetter <- function(agent,diagonal=FALSE){
  grid <- attr(agent,"grid")
  free <- freecells(grid)
  jumped <- FALSE
  free <- free[sample(1:length(free),length(free))]
  quality.own <- quality.cell(cell(agent),agent)
  for(cell in free){
    if(quality.cell(cell,agent)>quality.own){
      jumped <- TRUE
      moveto(agent,cell)
      break
    }
  }
  jumped
}

jumpatrandom <- function(agent,diagonal=FALSE){
  grid <- attr(agent,"grid")
  free <- freecells(grid)
  target <- free[[sample(1:length(free),1)]]
  moveto(agent,target)
  return(TRUE)
}

## run the simulation ##########################################################

if(exists("X11.options")){
  X11.orig.type <- X11.options()$type
  if(length(X11.orig.type))
    X11.options(type="Xlib")
}
par(ask=FALSE)

res <- Simulate(
  start={
    city <- newGrid(size,size)
    for(i in 1:n.agents){
      newGridAgent(sample(c("O","#"),1),city)
      }
    plot(city)
    city.start <- copyGrid(city)
    noop <- FALSE
  },
  step={
    if(!noop){
      jumped <- FALSE
      dissatisf <- dissatisfied.agents(city)
      dissatisf <- dissatisf[sample(1:length(dissatisf),length(dissatisf))]
      for(agent in dissatisf){
        jumpedOne <- jumpatrandom(agent)
        if(jumpedOne) plot(city)
        jumped <- jumped || jumpedOne
        }
      if(!jumped) noop <- TRUE
    }
    else interrupt("no change")
  },
  cleanup = retain(city,city.start),
  conditions=data.frame(
    size   =c(8,8,20,20),
    n.agents=c(20,60,200,350)
    ),
 nsim=100,
 keep.data=FALSE,
 keep.states=TRUE
)
if(exists("X11.options")){
  if(length(X11.orig.type))
    X11.options(type=X11.orig.type)
}


print(res)