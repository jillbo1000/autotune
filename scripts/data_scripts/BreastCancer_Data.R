
# BreastCancer: Wisconsin breast cancer data with 699 obs and 11 vars
# The final data have 10 variables and 683 observations
# The response is called Class, has two categories, and is a factor

library(mlbench) # BostonHousing, BreastCancer, Glass, Ionoshere, PimaIndiansDiabetes, Sonar, Soybean
data(BreastCancer)

BreastCancer <- BreastCancer[, c(11, 2:10)]
BreastCancer$Class <- as.factor(BreastCancer$Class)
# Data are factors so converted it to numeric
for(i in 2:10) {
  BreastCancer[, i] <- as.numeric(BreastCancer[, i])
}

bc <- BreastCancer[complete.cases(BreastCancer), ]
