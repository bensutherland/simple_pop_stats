# Uses a genepop file to run differentiation analyses
# Note: assumes populations are unique with only a single name, with no spaces
### Adjust pop names will only work with stock code and year if in the format for PBT, as per: stockcode_year_indivID_sex


#### 00. Front Matter ####
# Clear space
# rm(list=ls())

# Install packages
if (!require("adegenet")) install.packages("adegenet")
if (!require("hierfstat")) install.packages("hierfstat")
if (!require("phangorn")) install.packages("phangorn")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")

## Set working directory
current.path <- dirname(rstudioapi::getSourceEditorContext()$path)
current.path <- gsub(pattern = "\\/01_scripts", replacement = "", x = current.path) # take main directory
setwd(current.path)

## User set variables
datatype <- "SNP" # SNP or microsat ?
sep_by <- "collection" # sep_by collection or collection_and_year or none ?
name_by <- "stockname" # name_by stockcode or stockname # NOTE: stockname only compatible w/ sep_by "collection"
sc.fn <- "H:\\Stock_Codes\\eulachon\\euStockCodesCu.txt" # stock code filename


#### 01. Load in data ####
## Manually choose the file
# Function used will depends on using unix or windows
if(.Platform$OS.type == "unix") {
  my_genepop.path <- tk_choose.files(caption = "Select a genepop file" )
} else if(.Platform$OS.type == "windows") {
  my_genepop.path <- choose.files( default=file.path("02_raw_data/")
                          , caption = "Select a genepop file")
}

# Set allele.code
if(datatype=="SNP"){
  allele.code <- 2
}else if(datatype=="microsat"){
  allele.code <- 3
}

# Read in data
obj <- read.genepop(file = my_genepop.path
                    , ncode = allele.code)
obj


#### 01.1 Shorten pop names ####
# Extract genepop indiv names into df
indiv_names.df <- as.data.frame(rownames(obj$tab))
colnames(indiv_names.df) <- "indiv.name"
head(indiv_names.df)

# Separate the vector into individual components
indiv_names.df <- separate(data = indiv_names.df
                           , sep = "_"
                           , col = "indiv.name"
                           , into = c("stock.code", "year", "fish.id", "sex")
)
head(indiv_names.df)

# Rename indiv names as per user variable
if(sep_by=="collection"){
  
  # Reporting
  print("Renaming pops to collection")
  
  # Make new pop names
  pop_short <- str_pad(string = indiv_names.df$stock.code, width = 2, side = "left", pad = "0")
  
}else if(sep_by=="collection_and_year"){
  
  # Reporting
  print("Renaming pops to collection_and_year")
  
  # Make new pop names
  pop_short <- paste0(str_pad(string = indiv_names.df$stock.code, width = 2, side = "left", pad = "0"), "_", indiv_names.df$year)
  
}else {
  
  print("Not renaming pops")

}

pop(obj) <- pop_short # Change pop names
unique(pop(obj))

# Isolate again for future conversion
pop_short.df <- as.data.frame(pop(obj))
colnames(pop_short.df) <- "pop_short"
pop_short.df$pop_short <- as.integer(as.character(pop_short.df$pop_short)) # for below, this must be integer


#### 01.2 Pop names as numeric or character?
if(sep_by=="collection" && name_by=="stockname"){
  
  # Generate an order column for re-ordering the samples' stock code vector
  pop_short.df$id <- 1:nrow(pop_short.df)
  
  # Print
  print("Reading in stock codes")
  
  stock_codes.df <- read.delim2(file = sc.fn)
  
  # Merge to get the stock code name
  rosetta <- merge(x = stock_codes.df, y = pop_short.df, by.x = "Code", by.y = "pop_short", sort = F, all.y = T)
  
  print("re-ordering back into original order")
  rosetta <- rosetta[order(rosetta$id), ]
  
  # Use the collection instead of code
  pop(obj) <- rosetta$collection
  
  # Reporting
  print(unique(pop(obj)))
} else {
  print("Not renaming pop stock code to stock names")
}


#### 02. View data ####
# View features
dim(obj$tab) # How many ind? (rows) & how many marker-alleles ?
nPop(obj) # How many populations?
unique(pop(obj)) # What populations?
table(pop(obj)) # Sample size per population
table(pop(obj))[sort(names(table(pop(obj))))] # if numeric, this will help

# Plot sample size in baseline per population
fn <- paste0("03_results/sample_size_per_", sep_by, "_by_", name_by, ".pdf")
pdf(file = fn, width = 10, height = 5)
par(mar=c(8,5,3,3))
barplot(table(pop(obj))[sort(names(table(pop(obj))))]
        , las=2
        #, xlab="Stock_code"
        , ylab="Sample size"
        #, ylim = c(0,40)
        , main = basename(my_genepop.path)
        )
abline(h = c(30), lty=2)
dev.off()


## Could drop some pops here if they are too small
## Only if this is further sep by year, otherwise the dropping of pops should occur upstream

### 2b. Alleles per marker ###
table(nAll(obj))

# Drop monomorphic loci
drop_loci <- which(nAll(obj)==1)
obj_no_monomorphic <- obj[loc=-drop_loci]
all_data_no_monomorphic.hf <- genind2hierfstat(obj_no_monomorphic)


#### 03. Genetic Differentiation ####
# Change from genind to hierfstat
all.data.hf <- all_data_no_monomorphic.hf
dim(all.data.hf) # rows = samples; cols = markers with alelle ID (first col is pop)
rownames(all.data.hf) <- indNames(obj) # use indiv names as rownames

# Calculate pairwise wc fst
pairwise.wc.fst <- pairwise.WCfst(all.data.hf)
pairwise.wc.fst

# Save results
fn <- paste0("03_results/gen_diff_wcfst_", sep_by, "_by_", name_by, ".csv")
write.csv(pairwise.wc.fst, file = fn)


#### 04. Phylogenetic trees ####
#### 04.1. Based on wcfst ####
# Use UPGMA algorithm to estimate tree and plot
upgma.tree <- upgma(pairwise.wc.fst)

fn <- paste0("03_results/gen_diff_tree_upgma_", sep_by, "_by_", name_by, ".pdf")
pdf(file = fn, width = 7, height = 10)
plot(upgma.tree
     #, main = ""
     )
dev.off()

# Use NJ algorithm to estimate tree and plot
nj.tree <- NJ(pairwise.wc.fst)

fn <- paste0("03_results/gen_diff_tree_nj_", sep_by, "_by_", name_by, ".pdf")
pdf(file = fn, width = 7, height = 10)
plot(nj.tree
     # , main = ""
     )
dev.off()

# Use NJ algorithm (but unrooted) and plot
fn <- paste0("03_results/gen_diff_tree_nj_unroot_", sep_by, "_by_", name_by, ".pdf")
pdf(file = fn)
plot(nj.tree, "unrooted"
     #, main=""
     #, cex = 1.3
     )
dev.off()
# note, not as good as in FigTree

# Export the tree to produce more custom images in FigTree
fn <- paste0("03_results/gen_diff_tree_", sep_by, "_by_", name_by, ".tre")
write.tree(nj.tree, file=fn)

#### 04.1. Bootstrapping ####
par(mar=c(3,3,3,3))
pdf(file = "test.pdf")
bootstrapped_tree <- aboot(x = obj, dist = nei.dist, sample = 1000, strata = pop(obj))
dev.off()

bootstrapped_tree_UPGMA_cse <- aboot(x = obj, dist = edwards.dist, sample = 1000, strata = pop(obj))
write.tree(phy = bootstrapped_tree_nj_cse, file = "bootstrapped_tree_UPGMA_cse.tre")


bootstrapped_tree_nj_cse <- aboot(x = obj, tree = "nj", dist = edwards.dist, sample = 1000, strata = pop(obj))
write.tree(phy = bootstrapped_tree_nj_cse, file = "bootstrapped_tree_nj_cse.tre")

## Change to neighbour joining
## Use Corvelli - etc. if possible, if not WC FST
## screen out 0.5 axis label


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


## Things to record in logfile
# date
my_genepop.path
# github version ID
datatype
