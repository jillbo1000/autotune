library(dplyr)
library(tidyr)

# source("./scripts/functions/get_opt_data.R")
source("./scripts/functions/get_data.R")
source("./scripts/functions/get_eztune_results.R")
source("./scripts/functions/data_summary.R")
source("./scripts/functions/eztune_table.R")

data_sets <- c("Breast Cancer", "Ionosphere", "Pima", "Sonar", "Lichen", "Mullein")

# bring in eztune results
bc <- get_eztune_results("./data/eztune_data/BreastCancer", "Breast Cancer", "gbm")
io <- get_eztune_results("./data/eztune_data/Ionosphere", "Ionosphere", "gbm")
pi <- get_eztune_results("./data/eztune_data/Pima", "Pima", "gbm")
so <- get_eztune_results("./data/eztune_data/Sonar", "Sonar", "gbm")
li <- get_eztune_results("./data/eztune_data/Lichen", "Lichen", "gbm")
mu <- get_eztune_results("./data/eztune_data/Mullein", "Mullein", "gbm")


# bring in grid data
bcg <- datagridClass(filepath = "./data/grid_data/gbm/Binary/Breast Cancer", 
                     dataset = "Breast Cancer")
iog <- datagridClass("./data/grid_data/gbm/Binary/Ionosphere", 
                     dataset = "Ionosphere")
pig <- datagridClass("./data/grid_data/gbm/Binary/Pima", 
                     dataset = "Pima")
sog <- datagridClass("./data/grid_data/gbm/Binary/Sonar", 
                     dataset = "Sonar")
lig <- datagridClass("./data/grid_data/gbm/Binary/Lichen", 
                     dataset = "Lichen")
mug <- datagridClass("./data/grid_data/gbm/Binary/Mullein", 
                     dataset = "Mullein")

# get best grid results
best_results <- cbind.data.frame(rep("", 6), rep(0, 6), rep(0, 6))
colnames(best_results) <- c("Data", "MSE_UCL", "TimeUCL")
best_results$Data <- as.character(best_results$Data)

datasets <- list(bcg$datAll, iog$datAll, lig$datAll, mug$datAll, pig$datAll, sog$datAll)
for(i in 1:6) {
    best_results[i, 1] <- datasets[[i]]$cat[1]
    best_results[i, 2] <- min(datasets[[i]]$Error)
    best_results[i, 3] <- min(datasets[[i]]$Time)
}

best_results

# Bring in grid data to get best errors
# best_mse <- read.csv("best_mse.csv")

# summarize results by dataset
bc.sum <- make_summary_eztune_table2(bc)
bc.sum$perf_std <- bc.sum$mean_perf_cv - min(bc.sum$mean_perf_cv)
bc.sum$perf_std <- bc.sum$perf_std / max(bc.sum$perf_std)
bc.sum$time_std <- bc.sum$mean_time - min(bc.sum$mean_time)
bc.sum$time_std <- bc.sum$time_std/ max(bc.sum$time_std)

io.sum <- make_summary_eztune_table2(io)
io.sum$perf_std <- io.sum$mean_perf_cv - min(io.sum$mean_perf_cv)
io.sum$perf_std <- io.sum$perf_std / max(io.sum$perf_std)
io.sum$time_std <- io.sum$mean_time - min(io.sum$mean_time)
io.sum$time_std <- io.sum$time_std/ max(io.sum$time_std)

pi.sum <- make_summary_eztune_table2(pi)
pi.sum$perf_std <- pi.sum$mean_perf_cv - min(pi.sum$mean_perf_cv)
pi.sum$perf_std <- pi.sum$perf_std / max(pi.sum$perf_std)
pi.sum$time_std <- pi.sum$mean_time - min(pi.sum$mean_time)
pi.sum$time_std <- pi.sum$time_std/ max(pi.sum$time_std)

so.sum <- make_summary_eztune_table2(so)
so.sum$perf_std <- so.sum$mean_perf_cv - min(so.sum$mean_perf_cv)
so.sum$perf_std <- so.sum$perf_std / max(so.sum$perf_std)
so.sum$time_std <- so.sum$mean_time - min(so.sum$mean_time)
so.sum$time_std <- so.sum$time_std/ max(so.sum$time_std)

li.sum <- make_summary_eztune_table2(li)
li.sum$perf_std <- li.sum$mean_perf_cv - min(li.sum$mean_perf_cv)
li.sum$perf_std <- li.sum$perf_std / max(li.sum$perf_std)
li.sum$time_std <- li.sum$mean_time - min(li.sum$mean_time)
li.sum$time_std <- li.sum$time_std/ max(li.sum$time_std)

mu.sum <- make_summary_eztune_table2(mu)
mu.sum$perf_std <- mu.sum$mean_perf_cv - min(mu.sum$mean_perf_cv)
mu.sum$perf_std <- mu.sum$perf_std / max(mu.sum$perf_std)
mu.sum$time_std <- mu.sum$mean_time - min(mu.sum$mean_time)
mu.sum$time_std <- mu.sum$time_std/ max(mu.sum$time_std)


# combine datasets
std_dat <- rbind(bc.sum, io.sum, pi.sum, so.sum, li.sum, mu.sum)
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
gbm_bin_results <- results_table(dat = std_dat, best = best_results, 
                                 datasets = data_sets, model = "gbm")

write.table(gbm_bin_results[[1]], "./tables/tableB4_gbm_class_text.txt", 
            quote = FALSE,  col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(gbm_bin_results[[2]], "./tables/tableB4_gbm_class_latex.txt", 
            quote = FALSE, col.names = FALSE, row.names = FALSE, sep = "\t")



