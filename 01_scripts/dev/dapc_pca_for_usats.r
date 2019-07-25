# Do PCA on microsat data
levels(pop(x = obj_pop_filt_renamed))

data.hf <- genind2hierfstat(obj_pop_filt_renamed)
rownames(data.hf) <- indNames(obj_pop_filt_renamed)

## PCA ## on a matrix of individual genotype frequencies (hierfstat)
y <- indpca(data.hf, ind.labels = rownames(data.hf))

#filename <- paste(output.dir, datatype, "_sample_PCA.pdf", sep = "")
#pdf(file = filename, width = 11, height = 6)
plot(y, cex = 0.5)
dev.off()

# There is a weird one in there, lets get rid of it and re-run
data.hf[grep(pattern = "2013 3557", x = rownames(data.hf)),]

data_clean.hf <- data.hf[grep(pattern = "2013 3557", x = rownames(data.hf), invert = T),]

y_clean <- indpca(dat = data_clean.hf)

plot(y_clean)



# Do DAPC on microsat data
dapc <- dapc(obj_pop_filt_renamed, n.pca = 10, n.da = 1)

#filename <- paste(output.dir, datatype, "_sample_DAPC.pdf", sep = "")
#pdf(file = filename, width = 11, height = 6)
scatter(dapc, scree.da = F, bg = "white", legend = T
        #, txt.leg=rownames(dapc$means)
        , posi.leg = "topleft"
        , col = cols
)
#dev.off()