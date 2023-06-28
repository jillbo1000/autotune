# scripts directory contents
This directory contains the scripts used to generate all of the tables and figures in the article as well as example scripts for how the grid search and the eztune testing was done. 

## computation files
Contains scripts that were used to do the grid search and the eztune tests. The file names indicate what model type was tested and if the script is for creating the grid or the eztune tests. These scripts are examples of the actual scripts used to do the research. Many scripts were implemented and the results were obtained through parallel processing on a high performance computer cluster. If someone wished to reproduce the entire grid search or eztune tests, these scripts could be called in an HPC to create the data files. 

## data_scripts
Because the data were called so often and were used in so many different scripts, the data scripts in these files were called via a switch to ensure that the data were consistently formatted for computational ease. These scripts are used in nearly every script in this repository. 

## functions
This folder contains the helper functions that were used in the scripts to create the plots and tables for the articles. 

## performance_tables
Contains the scripts to create the eztune performance tables in Appendix B. The scripts create both a text table and a table formatted for latex. 

## plot_scripts
Contains the scripts used to create Figures 1-7 in the article. 

## install_packages.R
A script that will automatically install all of the packages needed to run the scripts in this repository. 
