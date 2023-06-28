wage <- read.table("./data/datasets/Wage.txt", header = TRUE)
wage <- wage[complete.cases(wage), ]
