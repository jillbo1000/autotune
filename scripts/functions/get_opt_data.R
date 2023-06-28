# Grabs data out of a folder of optimization results and summarizes it into a table

get_opt_data <- function(filepath, dataset) {
  allF <- list.files(filepath, pattern = ".(1|2).csv")
  dat <- NULL
  
  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    dat <- rbind(dat, read.csv(tmp))
  }
  
  dat$cat <- dataset
  
  dat
}


get_opt_data_All <- function(filepath, dataset) {
  allF <- list.files(filepath, pattern = ".csv")
  dat <- NULL
  
  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    dat <- rbind(dat, read.csv(tmp))
  }
  
  dat$cat <- dataset
  
  dat
}

check_dims <- function(filepath, dataset) {
  allF <- list.files(filepath, pattern = ".csv")

  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    dat <- read.csv(tmp)
    print(paste(allF[i], dim(dat)[1], dim(dat)[2], sep = " : "))
  }
}

get_opt_data_big <- function(filepath, dataset) {
  allF <- list.files(filepath, pattern = "(hjn|ga)")
  dat <- NULL
  
  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    dat <- rbind(dat, read.csv(tmp))
  }
  
  dat$cat <- dataset
  
  dat
}


