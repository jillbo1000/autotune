filepath <- "../Abalone"
dataset <- "Abalone"

get_eztune_results <- function(filepath, dataset, type) {
    
  allF0 <- list.files(filepath, pattern = type)
  
  allF <- allF0[c(grep("class", allF0), grep("mse", allF0))]
  dat <- NULL
  
  for(i in 1:length(allF)) {
    tmp <- paste(filepath, "/", allF[i], sep = "")
    tmp_dat <- read.csv(tmp)
    for(j in 1:5) {
      dat[, j] <- as.character(dat[, j])
    }
    dat <- rbind(dat, tmp_dat)
  }
  
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
