
interrupt <- function(msg=NULL)
      signalCondition(
        structure(list(message=msg),
            class=c("interrupt","condition")
          )
        )

Simulate <- function(
    step,
    conditions = NULL,
    start = NULL,
    cleanup = NULL,
    ...,
    nsim = 1,
    seed = NULL,
    trace=0,
    keep.data=TRUE,
    keep.states=FALSE,
    keep.seed = !is.null(seed),
    restore.seed = !is.null(seed),
    bucket=default_bucket
    ){
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    if(restore.seed)
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    if (is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
    }
    m <- match.call()
    dots <- list(...)
    bucket <- substitute(bucket)
    bucket <- if(is.character(bucket)) get(bucket,mode="function") else eval(bucket,parent.frame())
    results <- bucket(if(is.na(nsim))getOption("Simulation.chunk.size") else nsim)
    if(length(conditions)){
      conditions <- as.data.frame(conditions)
      states <- vector(mode="list",length=nrow(conditions))
      for(i in 1:nrow(conditions)){
          e <- new.env(parent=parent.frame())
          if(trace){
              cat("\n---------------------------------------------------\n")
              print(conditions[i,])
              }
          if(length(m$start)){
            start <- do.call("substitute",list(m$start,c(conditions[i,],dots)))
            dummy <- eval(start,envir=e)
          }
          step <- do.call("substitute",list(m$step,c(conditions[i,],dots)))
          step.vars <- all.vars(step)

          if(trace)
            fun <- function(.__repl){
              if(!(.__repl %% trace)) cat("Replication ",.__repl,"\n")
              eval(step,envir=e)
            }
          else
            fun <- function(.__repl) eval(step,envir=e)

          tryCatch(
            for(j in 1:nsim){
                res.j <- fun(j)
                if(keep.data && length(res.j)){
                  if(j == 1 && !length(names(res.j))){
                    if(length(res.j)==1) names(res.j) <- "result"
                    else names(res.j) <- paste("result",seq_along(res.j),sep=".")
                  }
                  put_into(results,c(conditions[i,],res.j))
                }
              },
            error=function(e)print(conditionMessage(e)),
            interrupt = function(i){
                if(length(msg <- conditionMessage(i))) cat(msg,"- ")
                else cat("interrupt - ")
                cat("finishing up\n")
                }
            )

          if(length(m$cleanup)){
            cleanup <- do.call("substitute",list(m$cleanup,c(conditions[i,],dots)))
            dummy <- eval(cleanup,envir=e)
          }
          if(keep.states)
            states[[i]] <- as.list(e)
      }
      if(keep.data){
        results <- pour_out(results)
        if(keep.states){
          results <- list(data=results,states=states)
        }
      }
      else if(keep.states)
          results <- states
      else results <- NULL
    }
    else {
      e <- new.env(parent=parent.frame())
      if(length(m$start)){
        start <- do.call("substitute",list(m$start,dots))
        dummy <- eval(start,envir=e)
      }
      step <- do.call("substitute",list(m$step,dots))
      step.vars <- all.vars(step)

      if(trace)
        fun <- function(.__repl){
          if(!(.__repl %% trace)) cat("Replication ",.__repl,"\n")
          eval(step,envir=e)
        }
      else
        fun <- function(.__repl) eval(step,envir=e)

      if(length(m$cleanup)){
        cleanup <- do.call("substitute",list(m$cleanup,c(conditions[i,],dots)))
        dummy <- eval(cleanup,envir=e)
      }
      results <- bucket(if(is.na(nsim))getOption("Simulation.chunk.size") else nsim)
        tryCatch(
          for(j in 1:nsim) {
              res.j <- fun(j)
              if(keep.data && length(res.j)){
                if(j == 1 && !length(names(res.j))){
                  if(length(res.j)==1) names(res.j) <- "result"
                  else names(res.j) <- paste("result",seq_along(res.j),sep=".")
                }
                put_into(results,res.j)
              }
          },
          error=function(e)print(conditionMessage(e)),
          interrupt = function(i){
              if(length(msg <- conditionMessage(i))) cat(msg,"- ")
              else cat("interrupt - ")
              cat("finishing replications\n")
              }
          )

      if(keep.data){
        results <- pour_out(results)
        if(keep.states){
          results <- list(data=results,states=as.list(e))
        }
      }
      else if(keep.states)
          results <- as.list(e)
      else results <- NULL
    }
    if(keep.seed)
      {
        if(is.null(results)) results <- list()
        attr(results,"seed") <- RNGstate
      }
    results
}

