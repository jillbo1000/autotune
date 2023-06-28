cv.pred <- function(x, y, fold, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  xval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:fold, length = nrow(x))
  xvs <- sample(xvs)
  cv.mse <- rep(0, fold)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm(y ~ ., distribution = "gaussian",
                 interaction.depth = id, n.trees = tr,
                 shrinkage = shr, data = train, n.minobsinnode = nmin)
    xval[xvs == i, 1] <- predict.gbm(gbm.t, newdata = test, type="response",
                                     n.trees = tr)
    cv.mse[i] <- mean((xval[xvs == i, 1] - xval[xvs == i, 2])^2)
  }
  mse <- mean((xval[, 1] - xval[, 2])^2)
  cv.pr <- list(mse = mse, cv.mse = cv.mse)
}
