
# output.dir <- "03_results/"


### TO BE INCORPORATED ###
# 1. Bootstrapped FST
# 2. DAPC
# 3. PCA
# 4. Loci contributing to DAPC



#### Things to record in logfile ####
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

