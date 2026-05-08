## Workflow to support population genetic analyses 
# Updated: 2026-05-08
# note: default updated to standalone (off-known network method), update variable on_network as needed

#### 00. Front Matter ####
# Clear space
# rm(list=ls())

## Install and load package managers
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# install.packages("devtools")
library(devtools)

# install.packages("remotes")
library(remotes)

## Install packages
# BiocManager::install("SNPRelate",force=TRUE)
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
# devtools::install_github("kkeenan02/diveRsity")
# install.packages("readr")  # reading data
# install.packages("vcfR")   # reading data
# install.packages("rubias")
# install.packages("rmarkdown")
# install.packages("gtools") # required for ask functionality
# devtools::install_version("Demerelate", version = "0.9-2") # for relatedness analyses
# install.packages("related", repos="http://R-Forge.R-project.org")
# install.packages("geosphere")

## Packages for baseline benchmark functionality
# install.packages("pander")
# install.packages("koRpus")
# install.packages("xfun") # (needs >= 0.19)
# install.packages("tinytex")
# tinytex::install_tinytex()
# install_tinytex()
# require(tinytex)
# tinytex::reinstall_tinytex()
# tinytex::tlmgr_install("pdfcrop")
# https://www.ghostscript.com/download/gsdnld.html

## Special note: for parallel processing of sims, require dev version of rubias
# remotes::install_github("eriqande/rubias", ref = "mclapply-assess-reference-loo")# install.packages("rmarkdown")

## Load packages
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
require("gtools")
require("readr")
require("rubias")
require("RGenetics")
require("diveRsity")
require("vcfR")
require("Demerelate")
require("related")
require("geosphere")
require("reshape2")
require("pander")
require("koRpus")
require("xfun")
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
# setup_network()  # function not currently operational
on_network <- FALSE # change to TRUE if working on-network at MGL


# User entry to set species for filling in variables
select_species()

