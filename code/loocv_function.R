library(SardineForecast)
loogam <- function(mod, train, k = 1, n = 100, LOO = FALSE, select = select, method = NULL) {
  dat1 <- train
  dat2 <- train # needed because gam and lm deal with predict data differently
  # to allow it to work on models with offset
  if (any(stringr::str_detect(colnames(dat1), "offset"))) {
    a <- colnames(dat1)
    a <- stringr::str_replace(a, "offset[(]", "")
    a <- stringr::str_replace(a, "[)]", "")
    colnames(dat1) <- a
    if (class(mod)[1] == "lm") colnames(dat2) <- a
  }
  if (any(stringr::str_detect(colnames(dat1), "poly"))) {
    a <- colnames(dat1)
    test1 <- stringr::str_detect(colnames(dat1), "poly")
    test2 <- stringr::str_detect(colnames(dat1), "raw = TRUE")
    if (!isTRUE(all.equal(test1, test2))) stop("If poly used in loogam model, then raw=TRUE must be used")
    for (i in which(test1 & test2)) {
      tmp <- dat1[, i][, 1, drop = FALSE]
      thename <- a[i]
      thename <- stringr::str_split(thename, "[(]")[[1]][2]
      thename <- stringr::str_split(thename, ",")[[1]][1]
      colnames(tmp) <- thename
      dat1 <- cbind(dat1, tmp)
      if (class(mod)[1] == "lm") dat2 <- cbind(dat2, tmp)
    }
  }

  if (class(mod)[1] == "gam") mod.formula <- mod$formula
  if (class(mod)[1] == "lm") mod.formula <- mod$terms
  pred <- actual <- aics <- aiccs <- r2s <- loos <- loomds <- NULL
  val <- utils::combn(dim(dat1)[1], k)

  if (n < ncol(val)) val <- val[, sample(ncol(val), n)]
  for (j in 1:ncol(val)) {
    i <- val[, j]
    if (class(mod)[1] == "gam") {
      m <- mgcv::gam(mod.formula,
        data = dat1[-1 * i, , drop = FALSE],
        select = select, method = method
      )
    }
    if (class(mod)[1] == "lm") m <- lm(mod.formula, data = dat1[-1 * i, , drop = FALSE])
    pred <- c(pred, exp(predict(m, newdata = dat2[i, , drop = FALSE])) * m$sig2 / 2)
    actual <- c(actual, dat1[i, 1])
    aics <- c(aics, AIC(m))
    aiccs <- c(aiccs, AICc(m))
    if (LOO) {
      lootmp <- loogam(m, train = train, k = k, n = n)
      loos <- c(loos, lootmp$RMSE)
      loomds <- c(loomds, lootmp$MdAE)
    }
    if (class(mod)[1] == "lm") r2s <- c(r2s, summary(m)$adj.r.squared)
    if (class(mod)[1] == "gam") r2s <- c(r2s, summary(m)$r.sq)
  }
  err <- pred - actual
  list(
    pred = pred, actual = actual,
    err = err, MAE = mean(abs(err)),
    RMSE = sqrt(mean(err^2)),
    MdAE = median(abs(err)),
    AIC = aics,
    AICc = aiccs,
    adj.r.sq = r2s,
    looRMSE = loos,
    looMdAE = loomds
  )
}
