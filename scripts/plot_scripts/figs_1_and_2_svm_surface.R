# set working directory to source file

library(ggplot2)
library(gridExtra)
library(tikzDevice)
library(RColorBrewer)

source("./scripts/functions/get_data.R")

bcg <- datagridClass(filepath = "./data/grid_data/svm/Binary/Breast Cancer", 
                     dataset = "Breast Cancer")
iog <- datagridClass("./data/grid_data/svm/Binary/Ionosphere", 
                     dataset = "Ionosphere")
pig <- datagridClass("./data/grid_data/svm/Binary/Pima", 
                     dataset = "Pima")
sog <- datagridClass("./data/grid_data/svm/Binary/Sonar", 
                     dataset = "Sonar")
lig <- datagridClass("./data/grid_data/svm/Binary/Lichen", 
                     dataset = "Lichen")
mug <- datagridClass("./data/grid_data/svm/Binary/Mullein", 
                     dataset = "Mullein")


#------------------------------------------------------------------------------
#                            Graphs for all data
#------------------------------------------------------------------------------

alldat <- rbind(bcg$datAll, iog$datAll, pig$datAll, sog$datAll, 
                lig$datAll, mug$datAll)

p1 <- ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  scale_fill_gradient2(low = "#08306b", mid = "#ffffbf", high = "#d73027", 
                       midpoint = median(alldat$Error, na.rm = TRUE)) +
  theme_bw() +
  labs(x = expression("Cost " ~(2^x)), y = expression(~gamma ~(2^y))) +
  xlim(-10, 25) +
  ylim(-25, 10) +
  ggtitle("All Errors for SVM Binary Data") +
  facet_wrap(~cat, ncol = 3)

pdf("./plots/fig_1a_svm_error_surface.pdf", height = 6, width = 9)
p1
dev.off()



#------------------------------------------------------------------------------
#                     Graphs for best 20 percent 
#------------------------------------------------------------------------------

dat20A <- rbind(bcg$dat20A, iog$dat20A, pig$dat20A, sog$dat20A, 
                lig$dat20A, mug$dat20A)

dat20LCL <- rbind(bcg$dat20LCL, iog$dat20LCL, pig$dat20LCL, sog$dat20LCL, 
                lig$dat20LCL, mug$dat20LCL)

dat20Time <- rbind(bcg$dat20Time, iog$dat20Time, pig$dat20Time, sog$dat20Time, 
                lig$dat20Time, mug$dat20Time)

dat10A <- rbind(bcg$dat10A, iog$dat10A, pig$dat10A, sog$dat10A, 
                lig$dat10A, mug$dat10A)

dat10LCL <- rbind(bcg$dat10LCL, iog$dat10LCL, pig$dat10LCL, sog$dat10LCL, 
                  lig$dat10LCL, mug$dat10LCL)

dat10Time <- rbind(bcg$dat10Time, iog$dat10Time, pig$dat10Time, sog$dat10Time, 
                   lig$dat10Time, mug$dat10Time)

dat5A <- rbind(bcg$dat5A, iog$dat5A, pig$dat5A, sog$dat5A, 
                lig$dat5A, mug$dat5A)

dat5LCL <- rbind(bcg$dat5LCL, iog$dat5LCL, pig$dat5LCL, sog$dat5LCL, 
                  lig$dat5LCL, mug$dat5LCL)

dat5Time <- rbind(bcg$dat5Time, iog$dat5Time, pig$dat5Time, sog$dat5Time, 
                   lig$dat5Time, mug$dat5Time)

best20A <- rbind(bcg$top20acc, iog$top20acc, pig$top20acc, sog$top20acc, 
                 lig$top20acc, mug$top20acc)

best20LCL <- rbind(bcg$top20LCL, iog$top20LCL, pig$top20LCL, sog$top20LCL, 
                   lig$top20LCL, mug$top20LCL)

best20Time <- rbind(bcg$top20Time, iog$top20Time, pig$top20Time, sog$top20Time, 
                    lig$top20Time, mug$top20Time)

p20 <- ggplot(dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  scale_fill_gradient2(low = "#08306b", mid = "#ffffbf", high = "#d73027", 
                       midpoint = median(alldat$Error, na.rm = TRUE)) +
  geom_point(data = best20A, aes(Cost, Gamma), 
             size = 3, fill = "orange1", color = "black", shape = 21) + 
  xlim(-10, 25) +
  ylim(-25, 10) +
  theme_bw() +
  labs(x = expression("Cost " ~(2^x)), y = expression(~gamma ~(2^y))) +
  ggtitle("Best 20% Errors for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 3)

pdf("./plots/fig_1b_svm_error20_surface.pdf", height = 6, width = 9)
p20
dev.off()


g <- NULL
cat2 <- c("Breast Cancer", "Ionosphere", "Lichen", "Mullein", "Pima", "Sonar")

for (i in seq_along(cat2)) {
  tmp <- dplyr::filter(alldat, cat == cat2[i])
  tmp20 <- dplyr::filter(best20Time, cat == cat2[i])
  # my_breaks <- unique(floor(exp(fivenum(log(tmp$Time)))))
  if (i < length(cat2)) rd <- 0 else rd <- 2
  my_breaks <- round(exp(seq(log(min(tmp$Time)), log(max(tmp$Time)), length.out = 5)), rd)
  g[[i]] <- ggplot(tmp, aes(x = Cost, y = Gamma)) +
    geom_tile(aes(fill = Time)) +
    # scale_fill_gradient2(low = "#08306b", mid = "#ffffbf", high = "#d73027", 
    #                      midpoint = median(tmp$Time, na.rm = TRUE), 
    #                      trans = "log", 
    #                      breaks = my_breaks) +
    scale_fill_gradientn(colors = rev(brewer.pal(5, "RdYlBu")), 
                         trans = "log", 
                         breaks = my_breaks
                         ) +
    geom_point(data = tmp20, aes(Cost, Gamma), 
               size = 3, fill = "orange1", color = "black", shape = 21) + 
    theme_bw() +
    # labs(x = bquote("Cost (2^x)"), y = "Gamma (2^y)") +
    labs(x = expression("Cost " ~(2^x)), y = expression(~gamma ~(2^y))) +
    xlim(-10, 25) +
    ylim(-25, 10) +
    ggtitle(paste(cat2[i], "Time Surface")) #+
    # facet_wrap(~cat, ncol = 1)
}

pdf("./plots/fig2_svm_time_surface.pdf", height = 9, width = 12)
grid.arrange(grobs = g, ncol = 3)
dev.off()



