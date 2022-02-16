# Execute a PCA from a genind file
# Will save out a plot into the result path

pca_from_genind <- function(data = obj_pop_filt, PCs_ret = 3
                            , plot_eigen = TRUE, plot_allele_loadings = TRUE
                            , colour_file = NULL
                            , retain_pca_obj = FALSE){
  
  print("Converting genind to genlight")
    
  # Convert genind to genlight using dartR
  obj.gl <- gi2gl(data, parallel = TRUE)
  my.data <- obj.gl
  
  print(paste0("Executing PCA, retaining ", PCs_ret, " PCs"))
        
  # Perform PCA
  pca1 <- glPca(my.data, nf = PCs_ret)
  
  
  # As required, save out pca1 obj to retain data
  if(retain_pca_obj == TRUE){
    
    print("Keeping pca.obj in global enviro")
    assign(x = "pca.obj", value = pca1, envir = .GlobalEnv)
    
    print("Writing out per sample PC loading values")
    pca_scores_per_sample.df <- as.data.frame(pca.obj$scores)
    # pca_scores_per_sample.df$sample <- rownames(pca_scores_per_sample.df)
    pca_scores_per_sample.df <- cbind(rownames(pca_scores_per_sample.df), pca_scores_per_sample.df)
    colnames(pca_scores_per_sample.df)[1] <- "sample"
    head(pca_scores_per_sample.df)
    
    write_tsv(x = pca_scores_per_sample.df, file = paste0(result.path, "pca_scores_per_sample.txt"))
    
  }else{
    
    print("Not retaining pca.obj, only running outputs")
    
  }

    
  # Set colour details  
  if(is.null(colour_file)){
    
    if(nPop(my.data)>1){
    
    # Define non-custom colours
    cols1 <- palette(rainbow(nPop(my.data)))
    
    } else {
      
      cols1 <- "blue"
      
    }

    # Create df with the colours
    colours.df <- as.data.frame(cbind(sort(unique(as.character(my.data$pop))),cols1))
    colnames(colours.df) <- c("collection","colour")

  } else if(!is.null(colour_file)){
    
    # Reporting
    print(paste0("Using custom colours file from ", colour_file))
    
    # Input colour file
    colours.df <- read.table(file = colour_file, header = T, sep = ",", stringsAsFactors = F)
    
  }
  
  
  # Plot w/ ggplot
  pca.scores <- as.data.frame(pca1$scores) # make dataframe for ggplot
  pca.scores$pop <- as.character(pop(data))
  pca.scores <- pca.scores[order(pca.scores$pop),] # add population to df
  head(pca.scores)
  
  # Create an eigenvalues df for plotting eigenvalues
  eig <- as.data.frame(pca1$eig)
  colnames(eig) <- "eig"
  
  
  # Bring in colours (old method, works well for base scatterplot)
  # pca.scores <- merge(x = pca.scores, y = colours, by.x = "pop", by.y = "collection", all.x = T, sort = F)
  # head(pca.scores)
  
  ## Create a colours vector
  # Select only the pops that are in the data
  library(dplyr)
  colours <- filter(colours.df, collection %in% unique(pca.scores$pop))
  
  # Put into alphabetic order by collection (the plotting order, and legend order)
  colours <- colours[order(colours$collection), ]
  
  # Take only the colour (note: must NOT be factor to correctly plot)
  ordered_colours <- as.character(colours$colour)
  
  
  ## Plot the PCA
  # PC1 vs. PC2
  set.seed(9)
  p <- ggplot(pca.scores, aes(x=PC1, y=PC2, colour=pop))
  p <- p + geom_point(size=2)
  p <- p + stat_ellipse(level = 0.95, size = 1)
  p <- p + scale_color_manual(name = "collection", values = ordered_colours)
  p <- p + geom_hline(yintercept = 0) 
  p <- p + geom_vline(xintercept = 0) 
  p <- p + theme_bw()
  
  p
  
  # Save out
  pdf(file = paste0(result.path, "pca_samples_PC1_v_PC2.pdf"), width = 11.5, height = 7.5)
  print(p)
  dev.off()
 
  # Determine PCs to include depending on how many PCs retained
  if(PCs_ret == 3){
    
    set.seed(9)
    p <- ggplot(pca.scores, aes(x=PC1, y=PC3, colour=pop))
    p <- p + geom_point(size=2)
    p <- p + stat_ellipse(level = 0.95, size = 1)
    p <- p + scale_color_manual(name = "collection", values = ordered_colours)
    p <- p + geom_hline(yintercept = 0) 
    p <- p + geom_vline(xintercept = 0) 
    p <- p + theme_bw()
    
    p
    
    # Save out
    pdf(file = paste0(result.path, "pca_samples_PC1_v_PC3.pdf"), width = 11.5, height = 7.5)
    print(p)
    dev.off()

  }else if(PCs_ret > 3){
    
    set.seed(9)
    p <- ggplot(pca.scores, aes(x=PC3, y=PC4, colour=pop))
    p <- p + geom_point(size=2)
    p <- p + stat_ellipse(level = 0.95, size = 1)
    p <- p + scale_color_manual(name = "collection", values = ordered_colours)
    p <- p + geom_hline(yintercept = 0) 
    p <- p + geom_vline(xintercept = 0) 
    p <- p + theme_bw()
    
    p
    
    # Save out
    pdf(file = paste0(result.path, "pca_samples_PC3_v_PC4.pdf"), width = 11.5, height = 7.5)
    print(p)
    dev.off()
    
  }
  
  
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
  
  
  ## Plotting eigenvalues
  if(plot_eigen==TRUE){
    
    print("Plotting eigenvalues")
    # Plot eigenvalues
    pdf(file = paste0(result.path, "pca_eigenvalues.pdf"), width = 4, height = 4)
    barplot(pca1$eig, col = heat.colors(50), main = "PCA Eigenvalues")
    dev.off()
    }
  
  if(plot_allele_loadings==TRUE){
    
    print("Plotting allele loadings")
    num.retained.pcs <- length(dimnames(pca1$loadings)[[2]])
    
    # Plot loading values of the markers in the PCs
    pdf(file = paste0(result.path, "pc_loadings.pdf"), width = 8, height = 8)
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
}
