#!/usr/bin/env Rscript
library("R.utils")
library(EZtune)
library(EZtuneTest)

# =============================================================================
#                            Instructions
# 
# This is the code that was used to generate the EZtune tests. It uses a 
# function from the package EZtuneTest, which is available from github.
# The runs were done on a computer cluster and the eztune variables are passed
# as arguments to the script in the command line. 
# 
# =============================================================================
    
num <- cmdArgs()
num
data_name <- as.character(num[[1]])
data_name
method <- as.character(num[[2]])
method
optimizer <- as.character(num[[3]])
optimizer
fast <- as.numeric(num[[4]])
fast
cross <- as.character(num[[5]])
cross
loss <- as.character(num[[6]])
loss 
seed <- as.numeric(num[[7]])
seed

set.seed(seed)

if(cross != "Resub") {
  cross <- as.numeric(cross)
} else {
  cross <- NULL
}

iterations <- 10

if(fast == 0) fast <- FALSE
if(fast == 1) fast <- TRUE

source("./scripts/functions/data_load.R")

path <- paste("./data/eztune_data/", data_name, "/", sep = "")

eztune_results(x = x, y = y, data_name = data_name, method = method,
               optimizer = optimizer, fast = fast, cross = cross,
               loss = loss, iterations = iterations, path = path)
