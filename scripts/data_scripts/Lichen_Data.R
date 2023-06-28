# Lichen has 840 obs and 41 vars
# Remove PlotNum and only retain LobaOreg as response. 
# Resulting dataset has 840 obs and 34 variables
# lichenTest does not need any manipulation because the 
# extra vars will be ignored

library(EZtune)
data(lichen)
lichen <- lichen[, c(2, 9:41)]
lichen[, 1] <- as.factor(lichen[, 1])
data(lichenTest)
lichenTest$LobaOreg <- as.factor(lichenTest$LobaOreg)


