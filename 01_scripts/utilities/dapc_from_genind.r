# Execute a DAPC from a genind file
# Will save out a plot into 03_results

dapc_from_genind <- function(data = obj_pop_filt, plot_allele_loadings = TRUE){
  
  print("Converting genind to genlight")
  
  # Convert genind to genlight using dartR
  obj.gl <- gi2gl(data, parallel = TRUE)
  my.data <- obj.gl
  
  print(paste0("Executing DAPC, retaining ", PCs_ret, " PCs"))
  
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
  
  data.gid <- my.data.gid
  
  # Change from genind file to hfstat
  data.hf <- genind2hierfstat(data.gid)
  rownames(data.hf) <- indNames(data.gid)

  
  # DAPC
  dapc <- dapc(data.gid, n.pca = 10, n.da = 1)
  
  filename <- paste("03_results/", "sample_DAPC.pdf", sep = "")
  pdf(file = filename, width = 11, height = 6)
  scatter(dapc, scree.da = F, bg = "white", legend = T
          , txt.leg=rownames(dapc$means)
          , posi.leg = "topleft"
          , col = cols
  )
  dev.off()
  
  
  # Loading plot # Plot marker variance contribution to DAPC
  if(plot_allele_loadings==TRUE){
  filename <- paste("03_results/", "DAPC_loadings.pdf", sep = "")
  pdf(file = filename, width = 11, height = 4)
  par(mfrow=c(1,1), mar=c(3,4,3,3))
  loadingplot(dapc$var.contr, thres=1e-3, las = 1, xlab = "Loci", ylab = "Locus contribution", las = 1, main = "")
  dev.off()
  
  
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
  write.table(x = dapc_loadings_vals, file = paste0("03_results/", "dapc_loadings.csv")
              , sep = ",", quote = F, col.names = T, row.names = F)
  

  }
  
}
