library(dplyr)

make_summary_eztune_table <- function(dat) {
  
  dataset <- dat$data[1]
  
  dat_sum <- group_by(dat, method, optimizer, fast, cross) %>%
    summarize(Dataset = dataset, 
              mean_perf = mean(performance, na.rm = TRUE), 
              sd_perf = sd(performance, na.rm = TRUE),
              mean_perf_cv = mean(cv_perf, na.rm = TRUE), 
              sd_perf_cv = sd(cv_perf, na.rm = TRUE),
              ucl_perf =  mean(performance, na.rm = TRUE) + 1.96 * sd(performance, na.rm = TRUE) / sqrt(10), 
              ucl_perf_cv =  mean(cv_perf, na.rm = TRUE) + 1.96 * sd(cv_perf, na.rm = TRUE) / sqrt(10))
  
  dat_sum$type <- ifelse(dat_sum$fast != "FALSE", paste("Fast = ", dat_sum$fast, sep = ""), NA)
  dat_sum$type <- ifelse(dat_sum$cross != "NULL", paste("CV = ", dat_sum$cross, sep = ""), dat_sum$type)
  dat_sum$type <- ifelse(is.na(dat_sum$type), "Resub", dat_sum$type)
  dat_sum
  
}


make_summary_eztune_table2 <- function(dat) {
  
  dataset <- dat$data[1]
  
  dat <- dplyr::filter(dat, loss_type == "mse" | loss_type == "class")
  
  suppressWarnings(dat_sum <- group_by(dat, method, optimizer, fast, cross) |>
    summarize(Dataset = dataset, 
              mean_perf = mean(loss, na.rm = TRUE), 
              sd_perf = sd(loss, na.rm = TRUE),
              ucl_perf =  mean(loss, na.rm = TRUE) + 1.96 * sd(loss, na.rm = TRUE) / sqrt(10), 
              mean_perf_cv = mean(loss_mse_acc_10, na.rm = TRUE), 
              sd_perf_cv = sd(loss_mse_acc_10, na.rm = TRUE),
              ucl_perf_cv =  mean(loss_mse_acc_10, na.rm = TRUE) + 1.96 * sd(loss_mse_acc_10, na.rm = TRUE) / sqrt(10), 
              mean_time = mean(time, na.rm = TRUE), 
              sd_time = sd(time, na.rm = TRUE),
              ucl_time =  mean(time, na.rm = TRUE) + 1.96 * sd(time, na.rm = TRUE) / sqrt(10), 
              max_time = max(time, na.rm = TRUE),
              min_time = min(time, na.rm = TRUE),
              max_perf_cv = max(loss_mse_acc_10, na.rm = TRUE), 
              min_perf_cv = min(loss_mse_acc_10, na.rm = TRUE))
    )
  
  dat_sum$type <- ifelse(dat_sum$cross != "resub", paste("CV = ", dat_sum$cross, sep = ""), "Resub")
  dat_sum$type <- ifelse(dat_sum$fast != "FALSE", paste("Fast = ", dat_sum$fast, sep = ""), dat_sum$type)
  dat_sum
  
}

