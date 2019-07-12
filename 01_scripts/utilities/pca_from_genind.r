# Execute a PCA from a genind file
# Will save out a plot into 03_results

pca_from_genind <- function(data = obj_pop_filt, PCs_ret = 3, plot_eigen = TRUE, plot_allele_loadings = TRUE){
  
  print("Converting genind to genlight")
    
  # Convert genind to genlight using dartR
  obj.gl <- gi2gl(data, parallel = TRUE)
  my.data <- obj.gl
  
  print(paste0("Executing PCA, retaining ", PCs_ret, " PCs"))
        
  # Perform PCA
  pca1 <- glPca(my.data, nf = PCs_ret)
  
  
  # Set colours
  library(RColorBrewer)
  cols1 <- brewer.pal(n = nPop(my.data), name = "Paired")
  cols2 <- brewer.pal(n = (nPop(my.data)-length(cols1)), name = "Spectral")
  cols <- c(cols1, cols2)
  
  # Plot w/ ggplot
  pca.scores <- as.data.frame(pca1$scores) # make dataframe for ggplot
  pca.scores$pop <- pop(data) # add pop factor
  head(pca.scores)
  #todo: could use a supplied text file to add colour values to the dataframe
  
  eig <- as.data.frame(pca1$eig)
  colnames(eig) <- "eig"
  
  # Plot
  library(ggplot2)
  set.seed(9)
  p <- ggplot(pca.scores, aes(x=PC1, y=PC2, colour=pop))
  p <- p + geom_point(size=2)
  p <- p + stat_ellipse(level = 0.95, size = 1)
  p <- p + scale_color_manual(values = cols) 
  p <- p + geom_hline(yintercept = 0) 
  p <- p + geom_vline(xintercept = 0) 
  p <- p + theme_bw()
  
  p
  
  # Save
  pdf(file = paste0("03_results/", "pca_all_samples.pdf"), width = 11.5, height = 7.5)
  p
  dev.off()
  
  
  ### INSET # (still to be built)
  # require(grid)
  # vp <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.2)
  
  # Plot eigenvalues to determine how many PCs should be retained
  # barplot(pca1$eig, col = heat.colors(2), main = "PCA Eigenvalues")
  
  ## plot2 needs to be a barplot
  # ggplot(data = eig, aes(x = eig)) + geom_bar(aes(fill=eig))
  # 
  # then this gets added
  # print(plot2, vp = vp)
  # 
  
  if(plot_eigen==TRUE){
    
    print("Plotting eigenvalues")
    
  
  # Plot eigenvalues
  pdf(file = paste0("03_results/", "pca_eigenvalues.pdf"), width = 4, height = 4)
  barplot(pca1$eig, col = heat.colors(50), main = "PCA Eigenvalues")
  dev.off()
  }
  
  if(plot_allele_loadings==TRUE){
    
    print("Plotting allele loadings")
    num.retained.pcs <- length(dimnames(pca1$loadings)[[2]])
    
    # Plot loading values of the markers in the PCs
    pdf(file = paste0("03_results/", "pc_loadings.pdf"), width = 8, height = 8)
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

  }
  
  assign(x = "cols", value = cols, envir = .GlobalEnv)
  
}
