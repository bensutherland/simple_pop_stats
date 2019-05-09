# Uses a genepop file to run differentiation analyses
# Note: assumes populations are unique with only a single name, with no spaces
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
# install.packages("stringr")
# install.packages("tidyr")


require("units")
require("cluster")
require("adegenet")
require("hierfstat")
require("phangorn")
require("poppr")
require("SNPRelate")
require("stringr")
require("tidyr")
# require("dartR") # fails on windows
require(tcltk)


## Set working directory
current.path <- dirname(rstudioapi::getSourceEditorContext()$path)
current.path <- gsub(pattern = "\\/01_scripts", replacement = "", x = current.path) # take main directory
setwd(current.path)

## User set variables
datatype <- "SNP" # SNP or microsat ?
sep_by <- "collection" # sep_by collection or collection_and_year or none ?
name_by <- "stockname" # name_by stockcode or stockname # NOTE: stockname only compatible w/ sep_by "collection"
#sc.fn <- "H:\\Stock_Codes\\eulachon\\euStockCodesCu.txt" # stock code filename
sc.fn <- "/hdd/07_eulachon/euStockCodesCU.txt" # stock code filename
output.dir <- "03_results/"

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
# NJ algorithm (but unrooted)
nj.tree <- NJ(pairwise.wc.fst)
fn <- paste0("03_results/gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, ".pdf")
pdf(file = fn)
plot(nj.tree, "unrooted"
     #, main=""
     #, cex = 1.3
     )
dev.off()
# note, not as good as in FigTree

# Export the tree to produce more custom images in FigTree
fn <- paste0("03_results/gen_diff_tree_nj_wcfst_", sep_by, "_by_", name_by, ".tre")
write.tree(nj.tree, file=fn)

#### 04.1. Bootstrapped tree ####
dist.method <- "edwards.dist" # set distance method

par(mar=c(3,3,3,3))
fn <- paste0("03_results/gen_diff_tree_nj_", dist.method, "_", sep_by, "_by_", name_by, ".pdf")
pdf(file = fn)
bootstrapped_tree <- aboot(x = obj_no_monomorphic, dist = dist.method, sample = 10000, strata = pop(obj_no_monomorphic))
dev.off()

fn <- paste0("03_results/gen_diff_tree_nj_", dist.method, "_", sep_by, "_by_", name_by, ".tre")
write.tree(phy = bootstrapped_tree, file = fn)


# #### WORKING ON FINDING A NEW OPTION FOR BOOTSTRAPPING FST BELOW ####
# install.packages("diveRsity")
# library("diveRsity")
# 
# diff_stats <- diffCalc(infile = my_genepop.path, outfile = "my_results", fst = TRUE, pairwise = TRUE
#                        , bs_pairwise = TRUE, boots = 1000, ci_type = "individuals")

# #### 6. bootstrapping Fst  ####
# # requires latest hierfstat (v0.04-29) otherwise get error
# nboots <- 1000
# boot.fst.all <- boot.ppfst(dat = all.data.hf[,-1], nboot = nboots, quant = c(0.025,0.975))
# 
# # debug
# for(i in 3:ncol(all.data.hf)){
#   print(i)
#   j <- i + 1
#   boot.ppfst(dat = all.data.hf[,i:j], nboot = 4, quant = c(0.025,0.975))
# }
# 
# boot.fst.all <- boot.ppfst(dat = all.data.hf[,], nboot = nboots, quant = c(0.025,0.975))
# 
# 
# boot.fst.all
# # note that nboot = 1000 is about the same as nboot = 10,000 (very marginally different)
# 
# 
#  
# # Collect output
# lower.limit <- t(boot.fst.all$ll)
# upper.limit <- boot.fst.all$ul
# upper.limit[is.na(upper.limit)] <- 0
# lower.limit[is.na(lower.limit)] <- 0
# boot.fst.all.output <- upper.limit + lower.limit
# boot.fst.all.output
# 
# filename <- paste0("03_results/all_data_fst_nboot_", nboots, ".csv")
# write.csv(x = boot.fst.all.output, file = filename)
# 
# 
# # genet.dist requires a df containing pop of orgn as 1st col, multi-locus genos in following
# head(all.data.hf)


## Things to record in logfile
# date
my_genepop.path
# github version ID
datatype


#### 07. Principal components analysis (PCA) ####
# Convert from genind to genlight
obj.gl <- gi2gl(obj_no_monomorphic, parallel = TRUE)
my.data <- obj.gl

# Perform PCA
pca1 <- glPca(my.data, nf = 3)

# Plot PCA (axes 1,2,3)
pdf(file = paste0("03_results/", "pca_all_samples.pdf"), width = 11.5, height = 7.5)
par(mfrow=c(2,1))
scatter(x = pca1, posi = "topleft", xax = 1, yax = 2)
title("PC1 (x) vs PC2 (y)", adj = 1)

scatter(x = pca1, posi = "topleft", xax = 3, yax = 2) # show PC3 and PC4
title("PC3 (x) vs PC2 (y)", adj = 1)
dev.off()

# Plot allele loadings
num.retained.pcs <- length(dimnames(pca1$loadings)[[2]]) # how many axes were retained? 

# Plot loading values of the markers in the PCs
pdf(file = paste0(output.dir, "pc_loadings.pdf"), width = 8, height = 8)
par(mfrow=c(num.retained.pcs,1))
# Plot the loading values of the different markers into the PCA
for(i in 1:num.retained.pcs){
  loadingplot(x = pca1, axis = i
              , main = paste("PC",i,"_loadings_(alleles)", sep = "")
              #, threshold = quantile(x = pca1$loadings[,i], 0.8) # not working
              , threshold = 0.001
  )
}
dev.off()

# Plot PCA with samples coloured by PC
pdf(file = paste0(output.dir , "pca_colorplot.pdf"), width = 8, height = 8)
par(mfrow=c(1,1), mar = c(4,4,4,4))
myCol <- colorplot(pca1$scores, pca1$scores, transp=T, cex=4)
abline(h=0,v=0, col = "grey")
text(x = 2, y = 5, paste(labels=nLoc(my.data), "loci", sep = " "))
text(x = 2, y = 4, labels=paste("PC1=Red", "\n", "PC2=Green", "\n", "PC3=Blue"))
dev.off()

# Bring colorplot colors into the samples of the Neighbour-joining tree
pdf(file = paste0(output.dir, "njt_colorplot.pdf"), width = 11, height = 9)
plot(nj(D), type="fan", show.tip=T, cex =  0.75)
tiplabels(pch=20, col=myCol, cex=4)
dev.off()


### 5. Discriminant Analysis of Principal Components ####
# First separate populations
# Convert genlight to matrix
my.data.mat <- as.matrix(my.data)
my.data.mat[1:5,1:5]

# Translate the number of minor allele to genind format
my.data.mat[my.data.mat == 0] <- "1/1" #homozygote reference
my.data.mat[my.data.mat == 1] <- "1/2" #heterozygote
my.data.mat[my.data.mat == 2] <- "2/2" #homozygote alternate
my.data.mat[1:5,1:5]

# Convert matrix to genind
my.data.gid <- df2genind(my.data.mat, sep = "/", ploidy = 2) # convert df to genind

# Transfer pop attributes
pop(my.data.gid) <- pop(my.data) 

unique(pop(my.data.gid))

# Separate
sep.obj <- seppop(x = my.data.gid)
names(sep.obj)

# Repool
datatype.list <- list()
datatype.list[["all.gid"]] <- my.data.gid
datatype.list[["south.gid"]] <- repool(sep.obj$Sandy_R, sep.obj$Elwha_R
                                       , sep.obj$Cowlitz_R, sep.obj$Columbia_R
                                       , sep.obj$Fraser_R, sep.obj$Klamath_R
                                       )

datatype.list[["north.gid"]] <- repool(sep.obj$Bear_R, sep.obj$`Bella Coola_R`, sep.obj$Carroll_Cr, sep.obj$Ecstall_R
                                       , sep.obj$Falls_Cr, sep.obj$Kemano_R, sep.obj$Khyex_R, sep.obj$Kingcome_R
                                       , sep.obj$Kitimat_R, sep.obj$Klinaklini_R, sep.obj$Nass_R, sep.obj$Skeena_R, sep.obj$Unuk_R
                                       , sep.obj$Wannock_R
)

datatype <- "all.gid"
# datatype <- "south.gid"
# datatype <- "north.gid"

data.gid <- datatype.list[[datatype]]

# Change from genind file to hfstat
data.hf <- genind2hierfstat(data.gid)
rownames(data.hf) <- indNames(data.gid)

# PCA on a matrix of individual genotype frequencies (hierfstat)
y <- indpca(data.hf, ind.labels = rownames(data.hf))
#y <- indpca(data.hf, ind.labels = pop(data.gid)) # this allows to view the size class type, if wanted

filename <- paste(output.dir, datatype, "_sample_PCA.pdf", sep = "")
pdf(file = filename, width = 11, height = 6)
plot(y, cex = 0.7)
dev.off()

# DAPC
dapc <- dapc(data.gid, n.pca = 10, n.da = 1)

filename <- paste(output.dir, datatype, "_sample_DAPC.pdf", sep = "")
pdf(file = filename, width = 11, height = 6)
scatter(dapc, scree.da = F, bg = "white", legend = T
        , txt.leg=rownames(dapc$means)
        , posi.leg = "topleft"
        #, col = cols
)
dev.off()


# Loading plot # Plot marker variance contribution to DAPC
filename <- paste(output.dir, datatype, "_DAPC_loadings.pdf", sep = "")
pdf(file = filename, width = 11, height = 4)
par(mfrow=c(1,1), mar=c(3,4,3,3))
loadingplot(dapc$var.contr, thres=1e-3, las = 1, xlab = "Loci", ylab = "Locus contribution", las = 1, main = "")
dev.off()




####







output.dir <- "03_results/"
# Transforms data (centers), performs PCA, performs Linear Discriminant Analysis (LDA) with PCs, calc allele contributions
dapc1 <- dapc(my.data, n.pca = 10, n.da = 1) # n.pca = number axes to be retained; n.da = number of axes retained in Discriminant Analysis step
dapc1 # Provides information such as proportion conserved variance and object details

# Plot density plot of samples along discriminant function 1
pdf(file = paste0("03_results/", "dapc_all_pops.pdf"), width = 10.5, height = 7)
par(mfrow=c(1,1))


stock_codes.df

scatter(dapc1, scree.da = F, bg = "white", legend = T
        , txt.leg=rownames(dapc1$means)
        , posi.leg = "topright"
        #, col = my.cols
)
dev.off()

# Loading plot # Plot marker variance contribution to DAPC
par(mfrow=c(1,1), mar=c(5,5,3,3))

# Plot the loading values of the different markers into the DAPC
pdf(file = paste0(output.dir, "dapc_loadings.pdf"), width = 10.5, height = 7)
loadingplot(dapc1$var.contr, thres=1e-3, xlab = "Loci", ylab = "Locus contribution", las = 1
            , main = "")
# This plots names for any variable with contribution > 1e-3 (0.001)
dev.off()

# To get all data, change the threshold, (not for plotting)
# Note: this needs to be proven that these are indeed in the same order (index and names)
dapc_loadings_all <- loadingplot(dapc1$var.contr, threshold = 0)
dapc_loadings_vals <- dapc_loadings_all$var.values # obtain var.contr vals
names(dapc_loadings_vals) <- names(dapc1$pca.cent) # bring in loci names instead of just index
head(dapc_loadings_vals)
dapc_loadings_vals <- as.data.frame(dapc_loadings_vals) # make a df
head(dapc_loadings_vals)
colnames(dapc_loadings_vals) <- "var.contr" # rename column
head(dapc_loadings_vals)
dapc_loadings_vals$mname <- rownames(x = dapc_loadings_vals) # make rownames a vector within df
head(dapc_loadings_vals)

# Write out, without rownames
write.table(x = dapc_loadings_vals, file = paste0(output.dir, "dapc_loadings.csv")
            , sep = ",", quote = F, col.names = T, row.names = F)


