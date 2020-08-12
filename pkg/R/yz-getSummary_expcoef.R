getSummary_expcoef.default <- function (obj, alpha = 0.05, ...){
    res <- getSummary(obj=obj, alpha=alpha, ...)
    coef <- res$coef
    exp_coef <- coef
    exp_coef[,1,] <- exp(coef[,1,])
    # Based on the delta-method
    exp_coef[,2,] <- exp_coef[,1,]*coef[,2,]
    if(ncol(coef) ==6)
        exp_coef[,5:6,] <- exp(coef[,5:6,])
    res$coef <- exp_coef
    return(res)
}
