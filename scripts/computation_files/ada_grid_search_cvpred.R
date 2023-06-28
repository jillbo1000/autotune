cv.pred <- function(x, y, fold, nu, iter, maxd) {
  dat <- cbind(y, x)
  xval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:fold, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, fold)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", iter = iter,
                      nu = nu, data = train, control = rpart.control(maxdepth = maxd))
    xval[xvs == i, 1] <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
    cv.acc[i] <- mean(round(xval[xvs == i, 1]) == xval[xvs == i, 2])
  }
  acc <- mean(round(xval[, 1]) == xval[, 2])
  cv.pr <- list(acc = acc, cv.acc = cv.acc)
}
