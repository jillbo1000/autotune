#!/usr/bin/env Rscript
library("R.utils")
library(e1071)
library(AppliedPredictiveModeling)

num <- cmdArgs()
file_num <- as.character(num[[1]])
eps <- as.numeric(num[[2]])
range1 <- as.integer(num[[3]])
range2 <- as.integer(num[[4]])
num
eps
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

# my own caret type function for a sparse grid. 
param <- NULL
for(i in -10:25) {
  param <- rbind(param, cbind(i, -25:10, eps, NA, NA, NA))
}

colnames(param) <- c("Cost", "Gamma", "Epsilon", "MSE", "MSE_UCL", "Time")


# Abalone Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  pr <- NULL
  try(pr <- svm(Rings ~ ., data = abalone, gamma = 2^param[i, 2], 
                cost = 2^param[i, 1], epsilon = param[i, 3], cross = 10))
  
  if(!is.null(pr)) {
    param[i, 4] <- pr$tot.MSE
    param[i, 5] <- mean(pr$MSE) + 1.96 * sd(pr$MSE) / sqrt(length(pr$MSE))
  }
  t2 <- Sys.time()
  param[i, 6] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- paste("./data/grid_data/svm/Regression/Abalone/Abalone_Grid_", file_num, ".csv", sep = "")

write.csv(param, f1, row.names = FALSE)


