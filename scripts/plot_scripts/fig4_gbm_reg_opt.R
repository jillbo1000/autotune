# File grabs all of the data for each dataset and then creates a summary 
# table for each dataset. Tables can be used to make graphs of time/error
# comparisons

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(tidyr)

source("./scripts/functions/get_opt_data.R")
source("./scripts/functions/make_summary_table.R")
source("./scripts/functions/get_data.R")

# Retrieve optimization results

ab.dat <- get_opt_data("./data/optimization_data/gbm/Regression/Abalone/", 
                       "Abalone")
bh.dat <- get_opt_data("./data/optimization_data/gbm/Regression/Boston Housing/", 
                       "Boston Housing")
co.dat <- get_opt_data("./data/optimization_data/gbm/Regression/CO2/", 
                       "CO2")
cr.dat <- get_opt_data("./data/optimization_data/gbm/Regression/Crime/",
                       "Crime")
oh.dat <- get_opt_data("./data/optimization_data/gbm/Regression/Ohio Housing/", 
                       "Ohio Housing")
un.dat <- get_opt_data("./data/optimization_data/gbm/Regression/Union/", 
                       "Union")
wa.dat <- get_opt_data("./data/optimization_data/gbm/Regression/Wage/", 
                       "Wage")

# Correct error for ames datasets, should be Ames
oh.dat$cat <- "Ames Housing"

# read in grid data to get best errors

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

best_results <- cbind.data.frame(rep("", 7), rep(0, 7), rep(0, 7))
colnames(best_results) <- c("Data", "MSE_UCL", "TimeUCL")
best_results$Data <- as.character(best_results$Data)

datasets <- list(abg$datAll, bhg$datAll, cog$datAll, crg$datAll, ohg$datAll, ung$datAll, wag$datAll)
for(i in 1:7) {
  best_results[i, 1] <- datasets[[i]]$cat[1]
  best_results[i, 2] <- min(datasets[[i]]$MSE)
  best_results[i, 3] <- min(datasets[[i]]$Time)
}

# Correct error for ohio datasets, should be Ames
best_results$Data[best_results$Data == "Ohio Housing"] <- "Ames Housing"
best_results

# Make tables

#-------------------------------------------------------------------------
#
#                         Parallel Coordinate Plot
#
#-------------------------------------------------------------------------

# Transform data by subtracing the best error obtained on the grid, then
# dividing by the max value. The same is done for time but without the shift. 

head(ab.dat)
ab.dat$MSEStd <- (ab.dat$MSE - best_results[1, 2]) / max(ab.dat$MSE - best_results[1, 2])
ab.dat$TimeStd <- ab.dat$Time / max(ab.dat$Time)
summary(ab.dat)

bh.dat$MSEStd <- (bh.dat$MSE - best_results[2, 2]) / max(bh.dat$MSE - best_results[2, 2])
bh.dat$TimeStd <- bh.dat$Time / max(bh.dat$Time)
summary(bh.dat)

co.dat$MSEStd <- (co.dat$MSE - best_results[3, 2]) / max(co.dat$MSE - best_results[3, 2])
co.dat$TimeStd <- co.dat$Time / max(co.dat$Time)
summary(co.dat)

cr.dat$MSEStd <- (cr.dat$MSE - best_results[4, 2]) / max(cr.dat$MSE - best_results[4, 2])
cr.dat$TimeStd <- cr.dat$Time / max(cr.dat$Time)
summary(cr.dat)

oh.dat$MSEStd <- (oh.dat$MSE - best_results[5, 2]) / max(oh.dat$MSE - best_results[5, 2])
oh.dat$TimeStd <- oh.dat$Time / max(oh.dat$Time)
summary(oh.dat)

un.dat$MSEStd <- (un.dat$MSE - best_results[6, 2]) / max(un.dat$MSE - best_results[6, 2])
un.dat$TimeStd <- un.dat$Time / max(un.dat$Time)
summary(un.dat)

wa.dat <- wa.dat[wa.dat$MSE < 1e+200, ]
wa.dat$MSEStd <- (wa.dat$MSE - best_results[6, 2]) / max(wa.dat$MSE - best_results[6, 2])
wa.dat$TimeStd <- wa.dat$Time / max(wa.dat$Time)
summary(wa.dat)

dat_pcp <- rbind(ab.dat, bh.dat, co.dat, cr.dat, oh.dat, un.dat, wa.dat)
dat_pcp2 <- dat_pcp[grepl(".2", dat_pcp$Optimizer), ]
dat_pcp2$Optimizer <- gsub(".2", "", dat_pcp2$Optimizer)
# head(cbind(dat_pcp$Optimizer, as.character(grepl(".2", dat_pcp$Optimizer))))

pcp_sum <- group_by(dat_pcp2, Optimizer, cat) %>%
  summarize(Mean_Time = mean(Time, na.rm = TRUE), 
            SD_Time = sd(Time, na.rm = TRUE), 
            Time_UCL = mean(Time, na.rm = TRUE) + 1.96 * sd(Time, na.rm = TRUE), 
            Mean_MSE = mean(MSE, na.rm = TRUE), 
            SD_MSE = sd(MSE, na.rm = TRUE), 
            MSE_UCL = mean(MSE, na.rm = TRUE) + 1.96 * sd(MSE, na.rm = TRUE))

pcp_sum$MSEStd <- 0
pcp_sum$TimeStd <- 0
for(i in 1:7) {
  tmp <- pcp_sum[pcp_sum$cat == best_results$Data[i], ]
  pcp_sum$MSEStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                           (pcp_sum$MSE_UCL - best_results[i, 2]) / max(tmp$MSE_UCL - best_results[i, 2]), 
                           pcp_sum$MSEStd)
  pcp_sum$TimeStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                            (pcp_sum$Time_UCL) / max(tmp$Time_UCL), 
                            pcp_sum$TimeStd)
}

opt <- unique(pcp_sum$Optimizer)
category <- unique(pcp_sum$cat)
nw <- NULL

for(i in 1:length(unique(pcp_sum$Optimizer))) {
  tmp <- pcp_sum[pcp_sum$Optimizer == opt[i], ]
  vals <- rep(1, 8)
  for(j in 1:length(unique(pcp_sum$cat))) {
    if(!(category[j] %in% tmp$cat)) {
      nw <- rbind(nw, c(opt[i], category[j]))
    }
  }
}

ones <- matrix(1, nrow = nrow(nw), ncol = 8)
pcp1 <- cbind(data.frame(nw, ones))
colnames(pcp1) <- colnames(pcp_sum)
pcp_sum <- bind_rows(pcp_sum, pcp1)

class(pcp_sum) <- "data.frame"
pcp_sum <- pcp_sum[order(pcp_sum$Optimizer), ]

cols <- brewer.pal(10, "RdBu")[c(1:4, 7:10)]

pcp <- pcp_sum |>
  pivot_longer(cols = c("MSEStd", "TimeStd"), 
               names_to = "type", 
               values_to = "Performance") |>
  mutate(type = ifelse(type == "MSEStd", "Standardized MSE", "Standardized Computation Time"))
pcp$type <- factor(pcp$type, levels = c("Standardized MSE", "Standardized Computation Time"))

pcp

pdf("./plots/fig4_gbm_reg_opt.pdf", height = 4.3, width = 8)
ggplot(pcp, aes(x = Optimizer, y = Performance, group = cat)) +
  geom_path(aes(color = factor(cat)), alpha = 0.7, lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title = "GBM Regression", x = NULL, y = "Performance") +
  facet_wrap(~ type, ncol = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

