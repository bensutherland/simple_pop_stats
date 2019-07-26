# Execute a DAPC from a genind file
# Will save out a plot into 03_results

dapc_from_genind <- function(data = obj_pop_filt, plot_allele_loadings = TRUE
                             , colour_file = NULL){
  
  print("Executing DAPC")
  
  ## DAPC
  dapc <- dapc(data, n.pca = 10, n.da = 1)
  
  # Bring in colour data
  if(is.null(colour_file)){
    
    print("There is no colour file, so will use default")    
    
    # Set colours default
    library(RColorBrewer)
    cols1 <- brewer.pal(n = nPop(my.data), name = "Paired")
    cols2 <- brewer.pal(n = (nPop(my.data)-length(cols1)), name = "Spectral")
    cols <- c(cols1, cols2)
    
    ordered_colours <- cols
    
  } else if(!is.null(colour_file)){
    
    # Reporting
    print(paste0("Using custom colours file from ", colour_file))
    
    # Input colour file
    colours.df <- read.table(file = colour_file, header = T, sep = ",", stringsAsFactors = F)
    
    ## Create a colours vector
    # Select only the pops that are in the data
    colours <- filter(colours.df, collection %in% rownames(dapc$means))
    
    # Put into alphabetic order by collection (the plotting order, and legend order)
    colours <- colours[order(colours$collection), ]
    
    # Take only the colour (note: must NOT be factor to correctly plot)
    ordered_colours <- colours$colour
    
  }
  

  
  ## Plot DAPC
  filename <- paste("03_results/", "sample_DAPC.pdf", sep = "")
  pdf(file = filename, width = 11, height = 6)
  scatter(dapc, scree.da = F, bg = "white", legend = T
          , txt.leg=rownames(dapc$means)
          , posi.leg = "topleft"
          , col = ordered_colours
  )
  dev.off()
  
  
  # Loading plot # Plot marker variance contribution to DAPC
  if(plot_allele_loadings==TRUE){
  filename <- paste("03_results/", "DAPC_loadings.pdf", sep = "")
  pdf(file = filename, width = 11, height = 4)
  par(mfrow=c(1,1), mar=c(3,4,3,3))
  loadingplot(dapc$var.contr, thres=1e-3, las = 1, xlab = "Loci", ylab = "Locus contribution", las = 1, main = "")
  dev.off()
  
  
  dapc_loadings_all <- loadingplot(dapc$var.contr, threshold = 0)
  dapc_loadings_vals <- dapc_loadings_all$var.values # obtain var.contr vals
  names(dapc_loadings_vals) <- names(dapc$pca.cent) # bring in loci names instead of just index
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
