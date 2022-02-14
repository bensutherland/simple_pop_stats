# Uses a genepop file to run differentiation analyses
### Adjust pop names will only work with stock code and year if in the format for PBT, as per: stockcode_year_indivID_sex

#### 00. Front Matter ####
# Clear space
# rm(list=ls())

# Install packages

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("SNPRelate")

# BiocManager::install("qvalue") # req for dartR

# install.packages("units")
# install.packages("cluster")
# install.packages("adegenet")
# install.packages("hierfstat")
# install.packages("phangorn")
# install.packages("pegas")
# install.packages("poppr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("R.methodsS3")
# install.packages("dartR")
# install.packages("rubias")
# install.packages("RGenetics")
# install.packages("diveRsity")
 
## For parallel processing of sims, require dev version of rubias
# install.packages("remotes")
# require(remotes)
# remotes::install_github("eriqande/rubias", ref = "mclapply-assess-reference-loo")

# install.packages("rmarkdown")
# install.packages("tinytex")
# tinytex::install_tinytex()

## For ask functionality
# install.packages("gtools")

## For reading data
# install.packages("readr")

## For relatedness analyses
# install.packages("Demerelate")
# install.packages("related", repos="http://R-Forge.R-project.org")

## For <insert> analysis
# install.packages("geosphere")

## For baseline benchmark functionality
# install.packages("pander")
# install.packages("koRpus")
# install.packages("xfun") # (needs >= 0.19)

require("units")
require("cluster")
require("adegenet")
require("hierfstat")
require("phangorn")
require("pegas")
require("poppr")
require("SNPRelate")
require("stringr")
require("tidyr")
require("R.methodsS3")
require("dartR")
require("tcltk")
require("dplyr")
require("ggplot2")
library("gtools")
library("readr")
library("rubias")
library("RGenetics")
library("diveRsity")

require("Demerelate")
require("related")
library("geosphere")
library("reshape2")
library("pander")
library("koRpus")
library("xfun")
require("tinytex")


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

# User entry to determine if on local network or offline
# setup_network()  # function does not seem to work properly
on_network <- TRUE # change to FALSE if off-network

# User entry to set species for filling in variables
select_species()

