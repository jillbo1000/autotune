
# Ionosphere: 351 obs, 35 vars
# No NAs and no transformations that seem helpful, but V2 only has one value 
# so I removed it
# 
library(mlbench) # BostonHousing, BreastCancer, Glass, Ionoshere, PimaIndiansDiabetes, Sonar, Soybean
data(Ionosphere)
Ionosphere <- Ionosphere[, c(35, 1, 3:34)]


