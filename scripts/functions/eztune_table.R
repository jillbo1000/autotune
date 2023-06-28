# Will make a table that summarizes performance for each type of run

results_table <- function(dat, best, datasets, model) {
  
  best[, 1] <- gsub(" ", "", best[, 1])
  datasets <- gsub(" ", "", datasets)
  
  tab <- matrix(NA, nrow = length(levels(dat$type)), ncol = length(datasets) + 2)
  colnames(tab) <- c("Optimizer", "Type", datasets)
  tab[1:length(levels(dat$type)), 2] <- levels(dat$type)
  tab <- rbind(tab, tab)
  tab[1:length(unique(dat$type)), 1] <- "hjn"
  tab[(length(unique(dat$type)) + 1):nrow(tab), 1] <- "ga"
  # tab[nrow(tab), 1] <- "Best Grid"
  
  for(i in 1:nrow(tab)) {
    for(j in 3:ncol(tab)) {
      err <- NA
      try(err <- trim(dat$mean_perf_cv[(dat$optimizer == tab[i, 1] & 
                                            as.character(dat$type) == tab[i, 2] & 
                                            dat$Dataset == colnames(tab)[j])]), 
          silent = TRUE)
      try(tim <- round(dat$mean_time[(dat$optimizer == tab[i, 1] & 
                                          as.character(dat$type) == tab[i, 2] & 
                                          dat$Dataset == colnames(tab)[j])], 0), 
          silent = TRUE)
      if(!is.na(err)) {
        tab[i, j] <- paste("(", err, ", ", tim, "s)", sep = "")
      }
    }
  }
  
  tab[1:length(unique(dat$type)), 1] <- "Hooke-Jeeves"
  tab[(length(unique(dat$type)) + 1):nrow(tab), 1] <- "Genetic Algorithm"
  
  # bg <- c("Best Grid", "", rep("", ncol(tab) - 2))
  bg <- rep("", nrow(best))
  best[, 1] <- gsub(" ", "", best[, 1])
  for(i in 1:nrow(best)) {
    bg[i] <- trim(best[best$Data == datasets[i], colnames(best) == "MSE_UCL"])
  }
  bg <- c("Best Grid", "", bg)
  tab <- rbind(tab, bg)
  
  tab_tmp <- rbind(colnames(tab), tab)
  tab2 <- NULL
  breaks <- rep("&", nrow(tab_tmp))
  for(i in 1:ncol(tab_tmp)) {
    tab2 <- cbind(tab2, tab_tmp[, i], breaks)
  }
  tab2[, ncol(tab2)] <- rep("\\\\", nrow(tab2))
  
  colnames(tab2) <- paste0("r", 1:ncol(tab2))
  
  tabs <- list(tab, tab2)
  
  tabs
}

trim <- function(num) {
  if(num >= 10000) {
    num <- as.character(formatC(num, format = "e", digits = 2))
  } else if(num >= 1000) {
    num <- as.character(round(num, 0))
  } else if(num >= 100) {
    num <- as.character(round(num, 1))
  } else if(num >= 10) {
    num <- as.character(round(num, 2))
  } else if(num >= 1) {
    num <- as.character(round(num, 3))
  } else if(num >= 0.01) {
    num <- as.character(round(num, 4))
  } else {
    num <- as.character(formatC(num, format = "e", digits = 2))
  }
  
  num
}


