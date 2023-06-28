library(dplyr)
library(tidyr)

# source("./scripts/functions/get_opt_data.R")
source("./scripts/functions/get_data.R")
source("./scripts/functions/get_eztune_results.R")
source("./scripts/functions/data_summary.R")
source("./scripts/functions/eztune_table.R")

data_sets <- c("Abalone", "BostonHousing", "CO2", "Crime", "AmesHousing", "Union", "Wage")

# bring in eztune results
ab <- get_eztune_results("./data/eztune_data/Abalone", "Abalone", "gbm")
bh <- get_eztune_results("./data/eztune_data/BostonHousing", "Boston Housing", "gbm")
co2 <- get_eztune_results("./data/eztune_data/CO2", "CO2", "gbm")
cr <- get_eztune_results("./data/eztune_data/Crime", "Crime", "gbm")
oh <- get_eztune_results("./data/eztune_data/AmesHousing", "Ames Housing", "gbm")
un <- get_eztune_results("./data/eztune_data/Union", "Union", "gbm")
wa <- get_eztune_results("./data/eztune_data/Wage", "Wage", "gbm")


# bring in grid data
abg <- datagridReg("./data/grid_data/gbm/Regression/Abalone", 
                   dataset = "Abalone")
bhg <- datagridReg("./data/grid_data/gbm/Regression/Boston Housing", 
                   dataset = "Boston Housing")
cog <- datagridReg("./data/grid_data/gbm/Regression/CO2", 
                   dataset = "CO2")
crg <- datagridReg("./data/grid_data/gbm/Regression/Crime", 
                   dataset = "Crime")
ohg <- datagridReg("./data/grid_data/gbm/Regression/Ohio Housing", 
                   dataset = "Ohio Housing")
ung <- datagridReg("./data/grid_data/gbm/Regression/Union", 
                   dataset = "Union")
wag <- datagridReg("./data/grid_data/gbm/Regression/Wage", 
                   dataset = "Wage")

# get best grid results
best_results <- cbind.data.frame(rep("", 7), rep(0, 7), rep(0, 7))
colnames(best_results) <- c("Data", "MSE_UCL", "TimeUCL")
best_results$Data <- as.character(best_results$Data)

datasets <- list(abg$datAll, bhg$datAll, cog$datAll, crg$datAll, ohg$datAll, ung$datAll, wag$datAll)
for(i in 1:7) {
    best_results[i, 1] <- datasets[[i]]$cat[1]
    best_results[i, 2] <- min(datasets[[i]]$MSE)
    best_results[i, 3] <- min(datasets[[i]]$Time)
}

# Correct error for ames datasets, should be Ames
best_results$Data[best_results$Data == "Ohio Housing"] <- "Ames Housing"
best_results

# Bring in grid data to get best errors
# best_mse <- read.csv("best_mse.csv")

# summarize results by dataset
ab.sum <- make_summary_eztune_table2(ab)
ab.sum$perf_std <- ab.sum$mean_perf_cv - min(ab.sum$mean_perf_cv)
ab.sum$perf_std <- ab.sum$perf_std / max(ab.sum$perf_std)
ab.sum$time_std <- ab.sum$mean_time - min(ab.sum$mean_time)
ab.sum$time_std <- ab.sum$time_std/ max(ab.sum$time_std)

bh.sum <- make_summary_eztune_table2(bh)
bh.sum$perf_std <- bh.sum$mean_perf_cv - min(bh.sum$mean_perf_cv)
bh.sum$perf_std <- bh.sum$perf_std / max(bh.sum$perf_std)
bh.sum$time_std <- bh.sum$mean_time - min(bh.sum$mean_time)
bh.sum$time_std <- bh.sum$time_std/ max(bh.sum$time_std)

co2.sum <- make_summary_eztune_table2(co2)
co2.sum$perf_std <- co2.sum$mean_perf_cv - min(co2.sum$mean_perf_cv)
co2.sum$perf_std <- co2.sum$perf_std / max(co2.sum$perf_std)
co2.sum$time_std <- co2.sum$mean_time - min(co2.sum$mean_time)
co2.sum$time_std <- co2.sum$time_std/ max(co2.sum$time_std)

cr.sum <- make_summary_eztune_table2(cr)
cr.sum$perf_std <- cr.sum$mean_perf_cv - min(cr.sum$mean_perf_cv)
cr.sum$perf_std <- cr.sum$perf_std / max(cr.sum$perf_std)
cr.sum$time_std <- cr.sum$mean_time - min(cr.sum$mean_time)
cr.sum$time_std <- cr.sum$time_std/ max(cr.sum$time_std)

oh.sum <- make_summary_eztune_table2(oh)
oh.sum$perf_std <- oh.sum$mean_perf_cv - min(oh.sum$mean_perf_cv)
oh.sum$perf_std <- oh.sum$perf_std / max(oh.sum$perf_std)
oh.sum$time_std <- oh.sum$mean_time - min(oh.sum$mean_time)
oh.sum$time_std <- oh.sum$time_std/ max(oh.sum$time_std)

un.sum <- make_summary_eztune_table2(un)
un.sum$perf_std <- un.sum$mean_perf_cv - min(un.sum$mean_perf_cv)
un.sum$perf_std <- un.sum$perf_std / max(un.sum$perf_std)
un.sum$time_std <- un.sum$mean_time - min(un.sum$mean_time)
un.sum$time_std <- un.sum$time_std/ max(un.sum$time_std)

wa.sum <- make_summary_eztune_table2(wa)
wa.sum$perf_std <- wa.sum$mean_perf_cv - min(wa.sum$mean_perf_cv)
wa.sum$perf_std <- wa.sum$perf_std / max(wa.sum$perf_std)
wa.sum$time_std <- wa.sum$mean_time - min(wa.sum$mean_time)
wa.sum$time_std <- wa.sum$time_std/ max(wa.sum$time_std)


# combine datasets
std_dat <- rbind(ab.sum, bh.sum, co2.sum, cr.sum, oh.sum, un.sum, wa.sum)
dim(std_dat)
summary(std_dat)
unique(std_dat$Dataset)
std_dat

# Set factor levels to make for better plotting
unique(std_dat$type)
type <- c("Resub", "CV = 10", "CV = 3", "Fast = TRUE", "Fast = 0.25", 
          "Fast = 0.5", "Fast = 0.75", "Fast = 250", "Fast = 50")  
std_dat$type <- factor(std_dat$type, levels = type)
std_dat <- std_dat[order(std_dat$type), ]
std_dat$Optim <- ifelse(std_dat$optimizer == "hjn", "Hooke-Jeeves", "Genetic Algorithm")

# generates a list with two tables. The first table is text, the second is in 
# latex format
gbm_reg_results <- results_table(dat = std_dat, best = best_results, 
                                 datasets = data_sets, model = "gbm")

write.table(gbm_reg_results[[1]], "./tables/tableB2_gbm_reg_text.txt", 
            quote = FALSE,  col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(gbm_reg_results[[2]], "./tables/tableB2_gbm_reg_latex.txt", 
            quote = FALSE, col.names = FALSE, row.names = FALSE, sep = "\t")

