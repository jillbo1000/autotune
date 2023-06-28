datagridClass <- function(filepath, dataset, pattern = NULL) {
  allF <- list.files(filepath, pattern = pattern)
  dat <- NULL
  
  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    dat <- rbind(dat, read.csv(tmp))
  }
  
  if ("Acc.LCL" %in% colnames(dat)) {
      colnames(dat)[grepl("Acc.LCL", colnames(dat))] <- "AccLCL"
  }
  
  dat$cat <- dataset
  if(max(dat$Accuracy > 1)) {
    dat$Accuracy <- dat$Accuracy * 0.01
    dat$AccLCL <- dat$AccLCL * 0.01
  } 
  
  dat$Error <- 1 - dat$Accuracy
  dat$ErrUCL <- 1 - dat$AccLCL
  
  dat20A <- dat[dat$Accuracy >= quantile(dat$Accuracy, 0.80), ]
  dat10A <- dat[dat$Accuracy >= quantile(dat$Accuracy, 0.90), ]
  dat5A <- dat[dat$Accuracy >= quantile(dat$Accuracy, 0.95), ]
  dat1A <- dat[dat$Accuracy >= quantile(dat$Accuracy, 0.99), ]

  dat20LCL <- dat[dat$AccLCL >= quantile(dat$AccLCL, 0.80), ]
  dat10LCL <- dat[dat$AccLCL >= quantile(dat$AccLCL, 0.90), ]
  dat5LCL <- dat[dat$AccLCL >= quantile(dat$AccLCL, 0.95), ]
  dat1LCL <- dat[dat$AccLCL >= quantile(dat$AccLCL, 0.99), ]
  
  dat20Time <- dat[dat$Time <= quantile(dat$Time, 0.20), ]
  dat10Time <- dat[dat$Time <= quantile(dat$Time, 0.10), ]
  dat5Time <- dat[dat$Time <= quantile(dat$Time, 0.05), ]
  dat1Time <- dat[dat$Time <= quantile(dat$Time, 0.01), ]
  
  top20acc <- dat[order(dat$Accuracy, decreasing = TRUE), ][1:20, ]
  top20LCL <- dat[order(dat$AccLCL, decreasing = TRUE), ][1:20, ]
  top20Time <- dat[order(dat$Time, decreasing = FALSE), ][1:20, ]
  
  list(dat20A = dat20A, dat10A = dat10A, dat5A = dat5A, dat1A = dat1A, 
       dat20LCL = dat20LCL, dat10LCL = dat10LCL, dat5LCL = dat5LCL, 
       dat1Time = dat1Time, dat20Time = dat20Time, dat10Time = dat10Time, 
       dat5Time = dat5Time, dat1Time = dat1Time, 
       top20acc = top20acc, top20LCL = top20LCL, top20Time = top20Time, 
       datAll = dat)
}





datagridClassGBM2 <- function(filepath1, filepath2, dataset) {
  allF1 <- list.files(filepath1)
  allF2 <- list.files(filepath2)
  
  dat <- NULL
  
  for(i in 1:length(allF1)) {
    tmp <- paste(filepath1, "/", allF1[i], sep = "")
    dat <- rbind(dat, read.csv(tmp))
  }
  
  for(i in 1:length(allF2)) {
    tmp <- paste(filepath2, "/", allF2[i], sep = "")
    dat <- rbind(dat, read.csv(tmp))
  }
  
  dat$cat <- dataset
  dat$Error <- 1 - dat$Accuracy
  dat$ErrUCL <- 1 - dat$Acc.LCL
  
  dat20A <- dat[dat$Accuracy > quantile(dat$Accuracy, 0.80), ]
  dat10A <- dat[dat$Accuracy > quantile(dat$Accuracy, 0.90), ]
  dat5A <- dat[dat$Accuracy > quantile(dat$Accuracy, 0.95), ]
  dat1A <- dat[dat$Accuracy > quantile(dat$Accuracy, 0.99), ]
  
  dat20LCL <- dat[dat$Acc.LCL > quantile(dat$Acc.LCL, 0.80), ]
  dat10LCL <- dat[dat$Acc.LCL > quantile(dat$Acc.LCL, 0.90), ]
  dat5LCL <- dat[dat$Acc.LCL > quantile(dat$Acc.LCL, 0.95), ]
  dat1LCL <- dat[dat$Acc.LCL > quantile(dat$Acc.LCL, 0.99), ]
  
  dat20Time <- dat[dat$Time < quantile(dat$Time, 0.20), ]
  dat10Time <- dat[dat$Time < quantile(dat$Time, 0.10), ]
  dat5Time <- dat[dat$Time < quantile(dat$Time, 0.05), ]
  dat1Time <- dat[dat$Time < quantile(dat$Time, 0.01), ]
  
  top20acc <- dat[order(dat$Accuracy, decreasing = TRUE), ][1:20, ]
  top20LCL <- dat[order(dat$Acc.LCL, decreasing = TRUE), ][1:20, ]
  top20Time <- dat[order(dat$Time, decreasing = FALSE), ][1:20, ]
  
  list(dat20A = dat20A, dat10A = dat10A, dat5A = dat5A, dat1A = dat1A, 
       dat20LCL = dat20LCL, dat10LCL = dat10LCL, dat5LCL = dat5LCL, 
       dat1Time = dat1Time, dat20Time = dat20Time, dat10Time = dat10Time, 
       dat5Time = dat5Time, dat1Time = dat1Time, 
       top20acc = top20acc, top20LCL = top20LCL, top20Time = top20Time, 
       datAll = dat)
}

datagridReg <- function(filepath, dataset) {
  allF <- list.files(filepath)
  dat <- NULL
  
  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    tmpDat <- read.csv(tmp)
    if(colnames(tmpDat)[6] == "MSE.UCL") {
      colnames(tmpDat)[6] <- "MSE_UCL"
    }
    # tmpDat <- tmpDat[complete.cases(tmpDat), ]
    tmpDat$MSE <- as.numeric(tmpDat$MSE)
    tmpDat$MSE_UCL <- as.numeric(tmpDat$MSE_UCL)
    if("Time" %in% colnames(tmpDat)) {
      dat <- rbind(dat, tmpDat)
    }
  }
  
  dat$cat <- dataset
  
  dat <- dat[!is.na(dat$MSE) & !is.na(dat$MSE_UCL) & dat$MSE < Inf & dat$MSE_UCL < Inf, ]

  dat20A <- dat[dat$MSE <= quantile(dat$MSE, 0.20, na.rm = TRUE), ]
  dat10A <- dat[dat$MSE <= quantile(dat$MSE, 0.10, na.rm = TRUE), ]
  dat5A <- dat[dat$MSE <= quantile(dat$MSE, 0.05, na.rm = TRUE), ]
  dat1A <- dat[dat$MSE <= quantile(dat$MSE, 0.01, na.rm = TRUE), ]
  
  dat20UCL <- dat[dat$MSE_UCL <= quantile(dat$MSE_UCL, 0.20, na.rm = TRUE), ]
  dat10UCL <- dat[dat$MSE_UCL <= quantile(dat$MSE_UCL, 0.10, na.rm = TRUE), ]
  dat5UCL <- dat[dat$MSE_UCL <= quantile(dat$MSE_UCL, 0.05, na.rm = TRUE), ]
  dat1UCL <- dat[dat$MSE_UCL <= quantile(dat$MSE_UCL, 0.01, na.rm = TRUE), ]
  
  dat20Time <- dat[dat$Time <= quantile(dat$Time, 0.20, na.rm = TRUE), ]
  dat10Time <- dat[dat$Time <= quantile(dat$Time, 0.10, na.rm = TRUE), ]
  dat5Time <- dat[dat$Time <= quantile(dat$Time, 0.05, na.rm = TRUE), ]
  dat1Time <- dat[dat$Time <= quantile(dat$Time, 0.01, na.rm = TRUE), ]
  
  top20acc <- dat[dat$MSE <= dat[order(dat$MSE, decreasing = FALSE), ]$MSE[20], ]
  top20UCL <- dat[dat$MSE_UCL <= dat[order(dat$MSE_UCL, decreasing = FALSE), ]$MSE_[20], ]
  top20Time <- dat[dat$Time <= dat[order(dat$Time, decreasing = FALSE), ]$Time[20], ]
  
  list(dat20A = dat20A, dat10A = dat10A, dat5A = dat5A, dat1A = dat1A, 
       dat20UCL = dat20UCL, dat10UCL = dat10UCL, dat5UCL = dat5UCL, 
       dat1Time = dat1Time, dat20Time = dat20Time, dat10Time = dat10Time, 
       dat5Time = dat5Time, dat1Time = dat1Time, 
       top20acc = top20acc, top20UCL = top20UCL, top20Time = top20Time, 
       datAll = dat)
}
