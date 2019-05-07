# Use a genepop file to run differentiation analyses
# Note: assumes populations are unique with only a single name, with no spaces
### Adjust pop names will only work with stock code and year if in the format for PBT, as per: stockcode_year_indivID_sex


# Clear space
# rm(list=ls())

## Install and load packages
# install.packages("adegenet")
# install.packages("hierfstat")
# install.packages("phangorn")
# install.packages("rstudioapi")
# install.packages("stringr")
# install.packages("tidyr")

library(adegenet)
library(hierfstat)
library(phangorn)
library(stringr)
library(tidyr)

# Set working directory
current.path <- dirname(rstudioapi::getSourceEditorContext()$path)
current.path <- gsub(pattern = "\\/01_scripts", replacement = "", x = current.path) # take main directory
setwd(current.path)

#### 1. Load in data ####
## Set variables 
# Set datatype
datatype <- "SNP"
#datatype <- "microsat"
if(datatype=="SNP"){
  allele.code <- 2
}else if(datatype=="microsat"){
  allele.code <- 3
}

# Set whether years should be treated differently
sep_by <- "collection_and_year"
#sep_by <- "collection"


## Manually choose the file
# Depends on unix or windows
if(.Platform$OS.type == "unix") {
  my_genepop.path <- tk_choose.files(caption = "Select a genepop file" )
} else if(.Platform$OS.type == "windows") {
  my_genepop.path <- choose.files( default=file.path("02_raw_data/")
                          , caption = "Select a genepop file")
}


## Read in data
obj <- read.genepop(file = my_genepop.path
                    , ncode = allele.code)
obj

### Adjust pop names
if(sep_by=="collection_and_year"){
  
  # Bring out vector of indiv names in the genepop
  indiv_names.df <- as.data.frame(rownames(obj$tab))
  colnames(indiv_names.df) <- "indiv.name"
  
  # Separate the vector into individual components
  indiv_names.df <- separate(data = indiv_names.df
                             , sep = "_"
                             , col = "indiv.name"
                             , into = c("stock.code", "year", "fish.id", "sex")
                             )
  
  # Recollect stock.code and year
  pop_short <- paste0(str_pad(string = indiv_names.df$stock.code, width = 2, side = "left", pad = "0"), "_", indiv_names.df$year)
  
  # Change popnames to the new indiv names
  pop(obj) <- pop_short
  
  # Report
  unique(pop(obj))
  
}else {
  print("Not changing popnames")
}

#TODO Rename pops?


#### 2. View data ####
dim(obj$tab) # How many ind ? (rows) & how many marker-alleles ?
nPop(obj) # How many populations?
unique(pop(obj)) # What populations?
table(pop(obj)) # Sample size per population
table(pop(obj))[sort(names(table(pop(obj))))]
# TODO: read in the stock code file so that the pops can be renamed as per strings


### 2a. Samples per population ###
# Plot sample size in baseline per population
pdf(file = "03_results/baseline_sample_size_per_pop_by_yr.pdf", width = 10, height = 5)
par(mar=c(5,5,3,3))
barplot(table(pop(obj))[sort(names(table(pop(obj))))]
        , col=funky(17)
        #, las=3, las = 1
        , las=2
        #, xlab="Stock_code"
        , ylab="Sample size"
        #, ylim = c(0,40)
        , main = basename(my_genepop.path)
        )
abline(h = c(30), lty=2)
dev.off()


### Could drop some pops here if they are too small



### 2b. Alleles per marker ###
table(nAll(obj))

# Drop loci that are not polymorphic
drop_loci <- which(nAll(obj)==1)
test <- obj[loc=-drop_loci]
all_data_no_monomorphic.hf <- genind2hierfstat(test)

#### 3. Calculate Fst ####
# Change from genind to hierfstat
all.data.hf <- genind2hierfstat(obj)
dim(all.data.hf) # rows = samples; cols = markers with alelle ID (first col is pop)
rownames(all.data.hf) <- indNames(obj) # use indiv names as rownames

#### TO DELETE ####
# # Debugging
# for(i in 2:ncol(all.data.hf)){
#   tryCatch({
#   print(paste("dropping col", i))
#   print( pairwise.WCfst(all.data.hf[, -i]) )
#   }, error=function(e){cat("Error :", conditionMessage(e), "\n")})
# }
#### END TO DELETE ####

# Calculate Weir & Cockerham Fst
# pairwise.wc.fst <- pairwise.WCfst(all.data.hf[, -2]) # during debugging
# pairwise.wc.fst <- pairwise.WCfst(all.data.hf[, -2]) # during debugging
# pairwise.wc.fst <- pairwise.WCfst(all.data.hf[c(1:100, 8100:8200), ])
pairwise.wc.fst <- pairwise.WCfst(all.data.hf)
pairwise.wc.fst

# Save results
if(sep_by=="collection_and_year"){
  FN <- "03_results/all_data_by_year_wcfst.csv"
} else {
  FN <- "03_results/all_data_wcfst.csv"
}
FN

write.csv(pairwise.wc.fst, file = FN)


#### 4. Plotting trees (and produce tree output) ####
# Use UPGMA algorithm to estimate tree and plot
upgma.tree <- upgma(pairwise.wc.fst)

pdf(file = "03_results/pairwise_wc_fst_tree_upgma_by_yr.pdf", width = 7, height = 10)
plot(upgma.tree
     #, main = ""
     )
dev.off()

# Use NJ algorithm to estimate tree and plot
nj.tree <- NJ(pairwise.wc.fst)

pdf(file = "03_results/pairwise_wc_fst_tree_nj.pdf", width = 7, height = 5)
plot(nj.tree
     # , main = ""
     )
dev.off()

# Use NJ algorithm (but unrooted) and plot
pdf(file = "03_results/pairwise_wc_fst_tree_nj_unrooted.pdf")
plot(nj.tree, "unrooted"
     #, main=""
     #, cex = 1.3
     )
dev.off()
# note, not as good as in FigTree

# Export the tree to produce more custom images in FigTree
write.tree(nj.tree, file="03_results/nj_tree.tre")


#### 5. Choosing the best tree model ####
# Not yet working to choose the best model (#todo#)
#parsimony(tree = nj.tree, data = )
# Look into here:
# http://adegenet.r-forge.r-project.org/files/PRstats/practical-introphylo.1.0.pdf
# https://www.molecularecologist.com/2016/02/quick-and-dirty-tree-building-in-r/



#### WORKING ON FINDING A NEW OPTION FOR BOOTSTRAPPING BELOW ####
install.packages("diveRsity")
library("diveRsity")

diff_stats <- diffCalc(infile = my_genepop.path, outfile = "my_results", fst = TRUE, pairwise = TRUE
                       , bs_pairwise = TRUE, boots = 1000, ci_type = "individuals")



#### 6. bootstrapping Fst  ####
# requires latest hierfstat (v0.04-29) otherwise get error
nboots <- 1000
boot.fst.all <- boot.ppfst(dat = all.data.hf[,-1], nboot = nboots, quant = c(0.025,0.975))

# debug
for(i in 3:ncol(all.data.hf)){
  print(i)
  j <- i + 1
  boot.ppfst(dat = all.data.hf[,i:j], nboot = 4, quant = c(0.025,0.975))
}

boot.fst.all <- boot.ppfst(dat = all.data.hf[,], nboot = nboots, quant = c(0.025,0.975))


boot.fst.all
# note that nboot = 1000 is about the same as nboot = 10,000 (very marginally different)



# Collect output
lower.limit <- t(boot.fst.all$ll)
upper.limit <- boot.fst.all$ul
upper.limit[is.na(upper.limit)] <- 0
lower.limit[is.na(lower.limit)] <- 0
boot.fst.all.output <- upper.limit + lower.limit
boot.fst.all.output

filename <- paste0("03_results/all_data_fst_nboot_", nboots, ".csv")
write.csv(x = boot.fst.all.output, file = filename)


# genet.dist requires a df containing pop of orgn as 1st col, multi-locus genos in following
head(all.data.hf)






# run figtree from linux:
# https://github.com/mooreryan/iroki_cli/wiki/Installing-FigTree
