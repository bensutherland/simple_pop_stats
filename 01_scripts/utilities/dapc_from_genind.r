# Execute a DAPC from a genind file
# Will save out a plot into 03_results

dapc_from_genind <- function(data = obj_pop_filt, plot_allele_loadings = TRUE
                             , colour_file = NULL
                             , n.pca = 10, n.da = 1
                             , scree.da = TRUE
                             , scree.pca = FALSE, posi.pca = "topright"
                             , dapc.width = 7, dapc.height = 5
                             ){
  
  print("Executing DAPC")
  
  ## DAPC
  dapc <- dapc(data, n.pca = n.pca, n.da = n.da)
  
  # Bring in colour data
  if(is.null(colour_file)){
    
    print("There is no colour file, so will use default")    
    
    # Set colours default
    library(RColorBrewer)
    cols1 <- brewer.pal(n = nPop(data), name = "Paired")
    cols2 <- brewer.pal(n = (nPop(data)-length(cols1)), name = "Spectral")
    cols <- c(cols1, cols2)
    
    ordered_colours <- cols
    
    # Only keep the required number of colours
    ordered_colours <- ordered_colours[1:nPop(data)]
    
    # Create df and vector as called below
    dapc_pops_colours.df <- matrix(nrow=length(ordered_colours), ncol = 1)
    dapc_pops_colours.df <- as.data.frame(dapc_pops_colours.df)
    colnames(dapc_pops_colours.df)[1] <- "colour"
    dapc_pops_colours.df$colour <- ordered_colours
    
  } else if(!is.null(colour_file)){
    
    # Reporting
    print(paste0("Using custom colours file from ", colour_file))
    
    # Input colour file
    colours.df <- read.table(file = colour_file, header = T, sep = ",", stringsAsFactors = F)
    
    ## Create a colours vector
    # Select only the pops that are in the data
    dapc_pops.df <- as.data.frame(rownames(dapc$means))
    colnames(dapc_pops.df) <- "pop" # note: this is the correct order
    
    # Merge the DAPC pops file with the colours file, maintaining order from pops file
    dapc_pops_colours.df <- merge(x = dapc_pops.df, colours.df, by.x = "pop", by.y = "collection", sort = F)
    
    # note: the colour must NOT be factor to correctly plot
    
  }
  

  
  ## Plot DAPC
  filename <- paste("03_results/", "sample_DAPC.pdf", sep = "")
  pdf(file = filename, width = dapc.width, height = dapc.height)
  scatter(dapc
          , col = dapc_pops_colours.df$colour
          , scree.da = scree.da
          , bg = "white", legend = T, txt.leg=rownames(dapc$means), posi.leg = "topleft"
          , scree.pca = scree.pca, posi.pca = posi.pca
  )
  dev.off()
  
  
  # Loading plot # Plot marker variance contribution to DAPC
  if(plot_allele_loadings==TRUE){
    
    # Set filename
    filename <- paste("03_results/", "DAPC_loadings.pdf", sep = "")
    
    # Plot
    # If there is only one DA, plot loadings for the DA, else
    if(dapc$n.da==1){
      
      pdf(file = filename, width = 11, height = 4)
      par(mfrow=c(1,1), mar=c(5,4,4,2) + 0.1)
      loadingplot(x = dapc$var.contr, thres = 1e-3
                  #, las = 1
                  , xlab = "Markers", ylab = "Locus contribution"
                  , main = "DF1 Variance Contribution"
      )
      dev.off()
      
    # If there are more than one DA, plot the first two
    }else if(dapc$n.da > 1){
      
      pdf(file = filename, width = 11, height = 8)
      par(mfrow=c(2,1), mar=c(5,4,4,2) + 0.1)
      loadingplot(x = dapc$var.contr, thres = 1e-3
                  , axis = 1
                  , xlab = "Markers", ylab = "Locus contribution", main = "DF1 Variance Contribution"
      )
      loadingplot(x = dapc$var.contr, thres = 1e-3
                  , axis = 2
                  , xlab = "Markers", ylab = "Locus contribution", main = "DF2 Variance Contribution"
      )
      dev.off()
      
    }

    
    # Obtain variance contribution values and their corresponding names
    dapc_var_contrib.df <- as.data.frame(dapc$var.contr)
    
    # Keep rownames in df
    dapc_var_contrib.df <- cbind(rownames(x = dapc_var_contrib.df), dapc_var_contrib.df) # make rownames a vector within df
    colnames(dapc_var_contrib.df)[1] <- "mname"
    head(dapc_var_contrib.df)
    
    # Write out, without rownames
    write.table(x = dapc_var_contrib.df, file = paste0("03_results/", "dapc_variance_contrib.csv")
                , sep = ",", quote = F, col.names = T, row.names = F
                )
  

  }
  
}
