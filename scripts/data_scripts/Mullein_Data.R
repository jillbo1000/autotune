
# Mullein has 12094 observations and 32 variables
# VerbThap is the class variable and is a factor
# There are no missing values# 

library(EZtune)
data(mullein)
mullein$VerbThap <- as.factor(mullein$VerbThap)
data(mulleinTest)
mulleinTest$VerbThap <- as.factor(mulleinTest$VerbThap)

