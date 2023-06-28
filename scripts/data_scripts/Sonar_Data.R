
# Sonar: 208 obs, 61 vars
# A bunch of the variables are skewed, but they aren't too bad so I am not going to 
# transform any of them.
# class is the respons variable

library(mlbench) # BostonHousing, BreastCancer, Glass, Ionoshere, PimaIndiansDiabetes, Sonar, Soybean
data(Sonar)
Sonar <- Sonar[, c(61, 1:60)]

