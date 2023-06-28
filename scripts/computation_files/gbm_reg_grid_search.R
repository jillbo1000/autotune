#!/usr/bin/env Rscript
library("R.utils")
library(gbm)
library(AppliedPredictiveModeling)

num <- cmdArgs()
file_num <- as.integer(num[[1]])
range1 <- as.integer(num[[2]])
range2 <- as.integer(num[[3]])
num
file_num
range1
range1


# =============================================================================
#                            Instructions
# 
# This is the code that was used to do the grid search. Getting the
# information for the entire grid at once is too computationally expensive, 
# so the grid was sent in pieces and the data were compiled later on. 
# The runs were done on a computer cluster and the variables are passed
# as arguments to the script in the command line. This particular script
# is for the Abalone data. The dataset can be altered.
# 
# =============================================================================


data(abalone)
abalone <- abalone[, c(9, 1:8)]

# source in cv.pred function
source("./scripts/computation_files/gbm_reg_grid_search_cvpred.R")

# my own caret type function for a sparse grid. 
p <- NULL
for(i in 1:40) {
  p <- rbind(p, cbind(i * 50, 5:12))
}

p2 <- NULL
for(i in 1:3) {
  p2 <- rbind(p2, cbind(p, -i))
}
head(p2)
tail(p2)

param <- NULL
for(i in 1:10) {
  param <- rbind(param, cbind(p2, (2 * i - 1), NA, NA, NA))
}

colnames(param) <- c("NumTrees", "MinNode", "Shrinkage", "IntDepth", "MSE", "MSE.UCL", "Time")

# Crime Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  pr <- NULL
  try(pr <- cv.pred(abalone[, -1], abalone[, 1], fold = 5, tr = param[i, 1], 
                    id = param[i, 4], nmin = param[i, 2], shr = param[i, 3]))
  
  if(!is.null(pr)) {
    param[i, 5] <- pr$mse
    param[i, 6] <- mean(pr$cv.mse) + 1.96 * sd(pr$cv.mse) / sqrt(length(pr$cv.mse))
  }
  t2 <- Sys.time()
  param[i, 7] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- paste("./data/grid_data/gbm/Regression/Abalone/Abalone_Small_Grid_", file_num, ".csv", sep = "")

write.csv(param, f1, row.names = FALSE)


