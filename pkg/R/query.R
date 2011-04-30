queryOne <- function(x,
                pattern,
                fuzzy=TRUE,
                perl=TRUE,
                fixed=TRUE,
                ignore.case=TRUE,
                insertions=0.999999999,
                deletions=0,
                substitutions=0
                ){
    max.distance <- list(insertions=insertions,deletions=deletions,substitutions=substitutions)
    found <- if(fuzzy)
                agrep(pattern,x,
                  ignore.case=ignore.case,
                  max.distance=max.distance)
             else
                suppressWarnings(grep(pattern,x,
                  perl=perl,
                  fixed=fixed,
                  ignore.case=ignore.case))
    if(length(found)) x else NULL
}

queryList <- function(x,
                pattern,
                fuzzy=TRUE,
                perl=TRUE,
                fixed=TRUE,
                ignore.case=TRUE,
                insertions=0.999999999,
                deletions=0,
                substitutions=0
                ){
   res <- lapply(x,query,
    pattern=pattern,
    fuzzy=fuzzy,
    perl=perl,
    fixed=fixed,
    ignore.case=ignore.case,
    insertions=insertions,
    deletions=deletions,
    substitutions=substitutions
   )
   res <- res[sapply(res,length)>0]
   if(length(res)) res else NULL
}


setMethod("query","data.set",function(x,pattern,...)queryList(x,pattern,...))
setMethod("query","importer",function(x,pattern,...)queryList(x,pattern,...))

setMethod("query","item",function(x,pattern,...){
  annot <- queryOne(annotation(x),pattern,...)
  labs <- queryOne(labels(x),pattern,...)
  if(length(annot) && length(labs)) list(annotation=annot, labels=labs)
  else if (length(annot)) annot
  else if (length(labs)) labs
  else NULL
})

