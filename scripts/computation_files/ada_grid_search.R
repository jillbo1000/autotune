#!/usr/bin/env Rscript
library("R.utils")
library(ada)
library(mlbench) # BostonHousing, BreastCancer, Glass, Ionoshere, PimaIndiansDiabetes, Sonar, Soybean

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


# Load all of the binary datasets for testing. The datasets are called
# bc, Ionosphere, pid, sonar, lich, mullein, titanic
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
source("./scripts/computation_files/ada_grid_search_cvpred.R")

# my own caret type function for a sparse grid. 
tmp <- c(0.01, 0.1, 0.3, 0.5, 0.7, 1)
p <- NULL
for(i in 1:length(tmp)) {
  p <- rbind(p, cbind(tmp[i], seq(100, 1400, 100)))
}

p2 <- NULL
tmp2 <- c(1:6, 8, 10, 12, 14, 16, 18, 20)
for(i in 1:length(tmp2)) {
  p2 <- rbind(p2, cbind(p, tmp2[i]))
}
head(p2)
tail(p2)

param <- NULL
for(i in 1:10) {
  param <- rbind(param, cbind(p2, NA, NA, NA))
}

colnames(param) <- c("Nu", "Iter", "Maxdepth", "Accuracy", "AccLCL", "Time")

# Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  try(pr <- cv.pred(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1, 
                    fold = 3, nu = param[i, 1], iter = param[i, 2], 
                    maxd = param[i, 3]))
  
  if(!is.null(pr)) {
    param[i, 4] <- pr$acc
    param[i, 5] <- mean(pr$cv.acc) - 1.96 * sd(pr$cv.acc) / sqrt(length(pr$cv.acc)) 
  }
  
  t2 <- Sys.time()
  param[i, 6] <- as.numeric(t2 - t1, units = "secs")
}

f3 <- "./data/grid_data/ada/"
f4 <- "_Grid_"

ff <- paste(f3, data_name, "/", data_name, f4, file_num, ".csv", sep = "")

write.csv(param, ff, row.names = FALSE)
