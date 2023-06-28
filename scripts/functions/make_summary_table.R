
# Grabs data out of a folder of optimization results and summarizes it into a table

filepath <- "C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Breast Cancer/"
dataset <- "Breast cancer"

library(dplyr)

make_summary_opt_table <- function(dat) {
  
  dat_sum <- group_by(dat, Optimizer) %>%
    summarize(Dataset = cat[1], 
              Mean_Time = mean(Time, na.rm = TRUE), 
              SD_Time = sd(Time, na.rm = TRUE), 
              Time_UCL = mean(Time, na.rm = TRUE) + 1.96 * sd(Time, na.rm = TRUE), 
              Mean_Error = mean(Error, na.rm = TRUE), 
              SD_Error = sd(Error, na.rm = TRUE), 
              Error_UCL = mean(Error, na.rm = TRUE) + 1.96 * sd(Error, na.rm = TRUE))
  
  dat_sum
  
}

make_summary_opt_table2 <- function(dat) {
  
  dat_sum <- group_by(dat, Optimizer) %>%
    summarize(Dataset = cat[1], 
              Mean_Time = mean(Time, na.rm = TRUE), 
              SD_Time = sd(Time, na.rm = TRUE), 
              Time_UCL = mean(Time, na.rm = TRUE) + 1.96 * sd(Time, na.rm = TRUE), 
              Mean_MSE = mean(MSE, na.rm = TRUE), 
              SD_MSE = sd(MSE, na.rm = TRUE), 
              MSE_UCL = mean(MSE, na.rm = TRUE) + 1.96 * sd(MSE, na.rm = TRUE))
  
  dat_sum
  
}