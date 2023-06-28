#!/usr/bin/env Rscript
library("R.utils")
library(gbm)

num <- cmdArgs()
data_name <- as.character(num[[1]])
file_num <- as.integer(num[[2]])
range1 <- as.integer(num[[3]])
range2 <- as.integer(num[[4]])
num
data_name
file_num
range1
range2

# =============================================================================
#                            Instructions
# 
# This is the code that was used to do the extensive grid search. Getting the
# information for the entire grid at once is too computationally expensive, 
# so the grid was sent in pieces and the data were compiled later on. 
# The runs were done on a computer cluster and the variables are passed
# as arguments to the script in the command line. 
# 
# =============================================================================


# Load the data

f1 <- "./scripts/data_scripts/"
f2 <- "_Data.R"

source(paste(f1, data_name, f2, sep = ""))

dat <- switch(data_name, 
              "BreastCancer" = bc, 
              "Ionosphere" = Ionosphere, 
              "Lichen" = lichen, 
              "Mullein" = mullein, 
              "Pima" = pid,
              "Sonar" = Sonar)

# cv.pred function
source("./scripts/computation_files/gbm_bin_grid_search_cvpred.R")

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

colnames(param) <- c("NumTrees", "MinNode", "Shrinkage", "IntDepth", "Accuracy", "Acc.LCL", "Time")

# Breast Cancer Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  pr <- NULL
  try(pr <- cv.pred(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1, fold = 10, tr = param[i, 1], 
                    id = param[i, 4], nmin = param[i, 2], shr = 10^param[i, 3]))
  
  if(!is.null(pr)) {
    param[i, 5] <- pr$acc
    param[i, 6] <- mean(pr$cv.acc) - 1.96 * sd(pr$cv.acc) / sqrt(length(pr$cv.acc))
  }
  t2 <- Sys.time()
  param[i, 7] <- as.numeric(t2 - t1, units = "secs")
}

f3 <- "./data/grid_data/gbm/Binary/"
f4 <- "_Grid_"

ff <- paste(f3, data_name, "2/", data_name, f4, file_num, ".csv", sep = "")

write.csv(param, ff, row.names = FALSE)
