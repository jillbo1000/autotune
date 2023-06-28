#!/usr/bin/env Rscript
library("R.utils")
library(e1071)
library(mlbench) # BostonHousing, BreastCancer, Glass, Ionoshere, PimaIndiansDiabetes, Sonar, Soybean

num = cmdArgs()
file_num = as.integer(num[[1]])
range1 = as.integer(num[[2]])
range2 = as.integer(num[[3]])
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
# is for the BreastCancer data. The dataset can be altered.
# 
# =============================================================================


# Load all of the binary datasets for testing. The datasets are called
# bc, Ionosphere, pid, sonar, lich, mullein, titanic
data(BreastCancer)
bc <- BreastCancer[, c(11, 2:10)]
bc$Class <- as.factor(bc$Class)
head(bc)
summary(bc)

# my own caret type function for a sparse grid. 
param <- NULL
for(i in -10:25) {
  param <- rbind(param, cbind(i, -25:10, NA, NA, NA))
}
colnames(param) <- c("Cost", "Gamma", "Accuracy", "AccLCL", "Time")

# Breast Cancer Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  try(mod <- svm(Class ~ ., data = bc, gamma = 2^param[i, 2], 
                 cost = 2^param[i, 1], cross = 10))
  
  if(!is.null(mod)) {
    param[i, 3] <- mod$tot.accuracy
    param[i, 4] <- mean(mod$accuracies) - 1.96 * sd(mod$accuracies) / sqrt(length(mod$accuracies)) 
  }
  
  t2 <- Sys.time()
  param[i, 5] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- paste("./data/grid_data/svm/Binary/BreastCancer/Breast_Cancer_Grid_", file_num, ".csv", sep = "")

write.csv(param, f1, row.names = FALSE)
