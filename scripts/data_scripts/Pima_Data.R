# PimaIndiansDiabetes: 768 obs, 9 vars
# No NAs
# Transform pregnant, pedigree, and age
# Returns pid

library(mlbench) # BostonHousing, BreastCancer, Glass, Ionoshere, PimaIndiansDiabetes, Sonar, Soybean
data(PimaIndiansDiabetes)
pid <- PimaIndiansDiabetes[, c(9, 1:8)]

