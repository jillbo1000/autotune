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

# Retrieve data

bc.dat <- get_opt_data(filepath = "./data/optimization_data/ada/Binary/BreastCancer/", 
                       dataset = "BreastCancer")
io.dat <- get_opt_data(filepath = "./data/optimization_data/ada/Binary/Ionosphere/", 
                       dataset = "Ionosphere")
li.dat <- get_opt_data(filepath = "./data/optimization_data/ada/Binary/Lichen/", 
                       dataset = "Lichen")
mu.dat <- get_opt_data(filepath = "./data/optimization_data/ada/Binary/Mullein/",
                       dataset = "Mullein")
pi.dat <- get_opt_data(filepath = "./data/optimization_data/ada/Binary/Pima/", 
                       dataset = "Pima")
so.dat <- get_opt_data(filepath = "./data/optimization_data/ada/Binary/Sonar/", 
                       dataset = "Sonar")

# Remove GA from Mullein datasets. It gave an error of 1 so 
# something is wrong there. 
mu.dat <- mu.dat[!grepl("ga", mu.dat$Optimizer), ]
summary(mu.dat)
mu.dat <- droplevels(mu.dat)
summary(mu.dat)

# Bring in grid data to get best errors

bcg <- datagridClass(filepath = "./data/grid_data/ada/Breast Cancer", 
                     dataset = "Breast Cancer")
iog <- datagridClass("./data/grid_data/ada/Ionosphere", 
                     dataset = "Ionosphere")
pig <- datagridClass("./data/grid_data/ada/Pima", 
                     dataset = "Pima")
sog <- datagridClass("./data/grid_data/ada/Sonar", 
                     dataset = "Sonar")
lig <- datagridClass("./data/grid_data/ada/Lichen", 
                     dataset = "Lichen")
mug <- datagridClass("./data/grid_data/ada/Mullein", 
                     dataset = "Mullein")

best_results <- cbind.data.frame(rep("", 6), rep(0, 6), rep(0, 6))
colnames(best_results) <- c("Data", "ErrorUCL", "TimeUCL")
best_results$Data <- as.character(best_results$Data)

datasets <- list(bcg$datAll, iog$datAll, lig$datAll, mug$datAll, pig$datAll, sog$datAll)
for(i in 1:6) {
  best_results[i, 1] <- datasets[[i]]$cat[1]
  best_results[i, 2] <- min(datasets[[i]]$Error)
  best_results[i, 3] <- min(datasets[[i]]$Time)
}


#-------------------------------------------------------------------------
#
#                         Parallel Coordinate Plot
#
#-------------------------------------------------------------------------

# Transform data by subtracing the best error obtained on the grid, then
# dividing by the max value. The same is done for time but without the shift. 

head(bc.dat)
bc.dat$ErrStd <- (bc.dat$Error - best_results[1, 2]) / max(bc.dat$Error - best_results[1, 2])
bc.dat$TimeStd <- bc.dat$Time / max(bc.dat$Time)
summary(bc.dat)

io.dat$ErrStd <- (io.dat$Error - best_results[2, 2]) / max(io.dat$Error - best_results[2, 2])
io.dat$TimeStd <- io.dat$Time / max(io.dat$Time)
summary(io.dat)

li.dat$ErrStd <- (li.dat$Error - best_results[3, 2]) / max(li.dat$Error - best_results[3, 2])
li.dat$TimeStd <- li.dat$Time / max(li.dat$Time)
summary(li.dat)

mu.dat$ErrStd <- (mu.dat$Error - best_results[4, 2]) / max(mu.dat$Error - best_results[4, 2])
mu.dat$TimeStd <- mu.dat$Time / max(mu.dat$Time)
summary(mu.dat)

pi.dat$ErrStd <- (pi.dat$Error - best_results[5, 2]) / max(pi.dat$Error - best_results[5, 2])
pi.dat$TimeStd <- pi.dat$Time / max(pi.dat$Time)
summary(pi.dat)

so.dat$ErrStd <- (so.dat$Error - best_results[6, 2]) / max(so.dat$Error - best_results[6, 2])
so.dat$TimeStd <- so.dat$Time / max(so.dat$Time)
summary(so.dat)

dat_pcp <- rbind(bc.dat, io.dat, li.dat, mu.dat, pi.dat, so.dat)
dat_pcp2 <- dat_pcp[grepl(".1", dat_pcp$Optimizer), ]
dat_pcp2$Optimizer <- gsub(".1", "", dat_pcp2$Optimizer)
# head(cbind(dat_pcp$Optimizer, as.character(grepl(".2", dat_pcp$Optimizer))))

pcp_sum <- group_by(dat_pcp2, Optimizer, cat) %>%
  summarize(Mean_Time = mean(Time, na.rm = TRUE), 
            SD_Time = sd(Time, na.rm = TRUE), 
            Time_UCL = mean(Time, na.rm = TRUE) + 1.96 * sd(Time, na.rm = TRUE), 
            Mean_Error = mean(Error, na.rm = TRUE), 
            SD_Error = sd(Error, na.rm = TRUE), 
            Error_UCL = mean(Error, na.rm = TRUE) + 1.96 * sd(Error, na.rm = TRUE))

pcp_sum$ErrStd <- 0
pcp_sum$TimeStd <- 0

best_results$Data <- gsub(" ", "", best_results$Data)

for(i in 1:6) {
  tmp <- pcp_sum[pcp_sum$cat == best_results$Data[i], ]
  pcp_sum$ErrStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                           (pcp_sum$Error_UCL - best_results[i, 2]) / max(tmp$Error_UCL - best_results[i, 2]), 
                           pcp_sum$ErrStd)
  pcp_sum$TimeStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                            (pcp_sum$Time_UCL) / max(tmp$Time_UCL), 
                            pcp_sum$TimeStd)
}

class(pcp_sum) <- "data.frame"

# Add in some values for the missing Mullein data

mullein_tmp <- as.data.frame(cbind(c("alo", "da", "ffa", "ga", "goa", "gwo", 
                                     "hjkb", "hs", "lbfgsb3", "mfo", "pso", 
                                     "sca", "spg", "woa"), "Mullein"))
mullein_tmp <- cbind(mullein_tmp, matrix(1, ncol = 8, nrow = nrow(mullein_tmp)))
colnames(mullein_tmp) <- colnames(pcp_sum)

# for(i in 1:10) {
#   if(i < 3) {
#     mullein_tmp[, i] <- as.character(mullein_tmp[, i])
#   } 
#   class(mullein_tmp[, i]) <- class(pcp_sum[, i])
# }

# mullein_tmp$ErrStd <- NA

pcp_sum2 <- rbind(pcp_sum, mullein_tmp)
pcp_sum2 <- pcp_sum2[order(pcp_sum2$Optimizer), ]

# cols <- brewer.pal(8, "Paired")[c(1:4, 7:8)]
# cols <- brewer.pal(8, "Dark2")[c(1:5, 8)]
cols <- brewer.pal(8, "PuOr")[c(1:3, 6:8)]
# cols <- brewer.pal(6, "PuOr")

pcp <- pcp_sum2 |>
  pivot_longer(cols = c("ErrStd", "TimeStd"), 
               names_to = "type", 
               values_to = "Performance") |>
  mutate(type = ifelse(type == "ErrStd", "Standardized Error", "Standardized Computation Time"))
pcp$type <- factor(pcp$type, levels = c("Standardized Error", "Standardized Computation Time"))

pcp

pdf("./plots/fig7_ada_bin_opt.pdf", height = 4.3, width = 8)
ggplot(pcp, aes(x = Optimizer, y = Performance, group = cat)) +
  geom_path(aes(color = factor(cat)), alpha = 0.7, lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title = "Adaboost Binary Classification", x = NULL, y = "Performance") +
  facet_wrap(~ type, ncol = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

