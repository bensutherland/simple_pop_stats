# Uses a genepop file to run differentiation analyses
### Adjust pop names will only work with stock code and year if in the format for PBT, as per: stockcode_year_indivID_sex

#### 00. Front Matter ####
# Clear space
# rm(list=ls())

# Install packages
#install.packages("BiocManager")
#BiocManager::install("qvalue") # req for dartR
#BiocManager::install("SNPRelate")

# install.packages("units")
# install.packages("cluster")
# install.packages("adegenet")
# install.packages("hierfstat")
# install.packages("phangorn")
# install.packages("poppr")
# install.packages("SNPRelate")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("dplyr")

# install.packages("Demerelate")
# install.packages("related", repos="http://R-Forge.R-project.org")

# install.packages("geosphere")


require("units")
require("cluster")
require("adegenet")
require("hierfstat")
require("phangorn")
require("poppr")
require("SNPRelate") # Failed on windows Not available Version 3.5.3 
require("stringr")
require("tidyr")
# require("dartR") # fails on windows
require(tcltk)
require(dplyr)
require(ggplot2)

# require("Demerelate")
# require("related")
library(geosphere)

## Set working directory
current.path <- dirname(rstudioapi::getSourceEditorContext()$path)
current.path <- gsub(pattern = "\\/01_scripts", replacement = "", x = current.path) # take main directory
setwd(current.path)

file_sources <- list.files(path = "01_scripts/utilities/", pattern = "\\.r$", full.names = TRUE, ignore.case = TRUE)

# Source functions
for(fun in file_sources){
  print(fun)
  source(fun)
  }
rm(fun, file_sources) # clean up


select_species()
