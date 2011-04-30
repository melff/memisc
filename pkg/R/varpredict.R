# orig.predict.lm <- get("predict.lm",pos="package:stats")
# orig.predict.glm <- get("predict.glm",pos="package:stats")

orig.predict.lm <- stats::predict.lm
orig.predict.glm <- stats::predict.glm

predict.lm <- function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
    interval = c("none", "confidence", "prediction"), level = 0.95,
    type = c("response", "terms","variables"), terms = NULL,
    variables=NULL, na.action = na.pass,
    pred.var = res.var/weights, weights = 1, ...){
    type <- match.arg(type)
    if(type != "variables"){
        m <- match.call()
        m$variables <- NULL
        m[[1]] <- orig.predict.lm
        return(eval(m,parent.frame()))
    }

    tt <- terms(object)
    if (missing(newdata) || is.null(newdata)) {
        mm <- X <- model.matrix(object)
        mmDone <- TRUE
        offset <- object$offset
    }
    else {
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
            xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts = object$contrasts)
        offset <- if (!is.null(off.num <- attr(tt, "offset")))
            eval(attr(tt, "variables")[[off.num + 1]], newdata)
        else if (!is.null(object$offset))
            eval(object$call$offset, newdata)
        mmDone <- FALSE
    }
    n <- length(object$residuals)
    p <- object$rank
    p1 <- seq_len(p)
    piv <- object$qr$pivot[p1]
    if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
        warning("prediction from a rank-deficient fit may be misleading")
    beta <- object$coefficients
    interval <- match.arg(interval)
    if (interval == "prediction") {
        if (missing(newdata))
            warning("Predictions on current data refer to _future_ responses\n")
        if (missing(newdata) && missing(weights)) {
            w <- weights(object)
            if (!is.null(w)) {
                weights <- w
                warning("Assuming prediction variance inversely proportional to weights used for fitting\n")
            }
        }
        if (!missing(newdata) && missing(weights) && !is.null(object$weights) &&
            missing(pred.var))
            warning("Assuming constant prediction variance even though model fit is weighted\n")
        if (inherits(weights, "formula")) {
            if (length(weights) != 2)
                stop("'weights' as formula should be one-sided")
            d <- if (missing(newdata) || is.null(newdata))
                model.frame(object)
            else newdata
            weights <- eval(weights[[2]], d, environment(weights))
        }
    }
    type <- match.arg(type)
    if (se.fit || interval != "none") {
        res.var <- if (is.null(scale)) {
            r <- object$residuals
            w <- object$weights
            rss <- sum(if (is.null(w))
                r^2
            else r^2 * w)
            df <- n - p
            rss/df
        }
        else scale^2
    }
#     if (type == "variables") {
        if (!mmDone) {
            mm <- model.matrix(object)
            mmDone <- TRUE
        }
        aa <- attr(mm, "assign")
        ll <- attr(tt, "term.labels")
        oo <- attr(tt, "order")
        hasintercept <- attr(tt, "intercept") > 0
        if (hasintercept) {
            if (!mmDone) {
                mm <- model.matrix(object)
                mmDone <- TRUE
            }
            avx <- colMeans(mm)
            termsconst <- sum(avx[piv] * beta[piv])
        }
        if(missing(variables))
            variables <- all.vars(delete.response(tt))
        else
            variables <- variables[variables %in% all.vars(tt)]
        nvars <- length(variables)
        if(nvars>0){
            vi <- sapply(variables,function(v)
                        which(sapply(ll,function(ll){
                            ll <- parse(text=ll)
                            v %in% all.vars(asOneSidedFormula(ll))
                            })),
                        simplify=FALSE
                    )
            vii <- structure(lapply(vi,function(i) which(aa %in% i)),names=names(vi))
            predictor <- matrix(ncol = nvars, nrow = NROW(X))
            dimnames(predictor) <- list(rownames(X), variables)
            if (se.fit || interval != "none") {
                ip <- matrix(ncol = nvars, nrow = NROW(X))
                dimnames(ip) <- list(rownames(X), variables)
                Rinv <- qr.solve(qr.R(object$qr)[p1, p1])
            }
            if (hasintercept)
                X <- sweep(X, 2, avx)
            unpiv <- rep.int(0, NCOL(X))
            unpiv[piv] <- p1
            for(i in 1:nvars){
                iipiv <- vii[[i]]
                ii <- unpiv[iipiv]
                iipiv[ii == 0] <- 0
                predictor[, i] <- if (any(iipiv > 0))
                  X[, iipiv, drop = FALSE] %*% beta[iipiv]
                else 0
                if (se.fit || interval != "none")
                  ip[, i] <- if (any(iipiv > 0))
                    as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii,, drop = FALSE])^2 %*% rep.int(res.var, p)
                  else 0
            }
        }
        else {
            predictor <- ip <- matrix(0, n, 0)
        }
        attr(predictor, "constant") <- if (hasintercept) termsconst
                                      else 0
    #}
    if (interval != "none") {
        tfrac <- qt((1 - level)/2, df)
        hwid <- tfrac * switch(interval, confidence = sqrt(ip),
            prediction = sqrt(ip + pred.var))
        lwr <- predictor + hwid
        upr <- predictor - hwid
    }
    if (se.fit || interval != "none")
        se <- sqrt(ip)
    if (missing(newdata) && !is.null(na.act <- object$na.action)) {
        predictor <- napredict(na.act, predictor)
        if (se.fit)
            se <- napredict(na.act, se)
    }
    if (interval != "none") {
        if (missing(newdata) && !is.null(na.act)) {
            lwr <- napredict(na.act, lwr)
            upr <- napredict(na.act, upr)
        }
        list(fit = predictor, se.fit = se, lwr = lwr, upr = upr,
            df = df, residual.scale = sqrt(res.var))
    }
    else if (se.fit)
        list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
    else predictor
}

predict.glm <- function (object, newdata = NULL, type = c("link", "response",
    "terms","variables"), se.fit = FALSE, dispersion = NULL, terms = NULL,
    na.action = na.pass, ...){

  type <- match.arg(type)
  m <- match.call()
  if(type == "variables"){
    m[[1]] <- predict.lm
    m$scale <- 1
  } 
  else
    m[[1]] <- orig.predict.glm
  return(eval(m,parent.frame()))
}

